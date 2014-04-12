-module(tentacles_dispatcher).

-behaviour(gen_server).

-export([start_link/2, sync_message/4, async_message/4, is_alive/3, ping/2, expire/2,
         change_timeout/2, get_timeout/1, send_event_to_controller/4,
         send_event/3, whois_broadcast/1, stop/2]).

-export([get_dispatcher_module/1, get_controller_module/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(TENTACLES_CONTROLLER_MAX_AGE, 30000).

-define(TENTACLES_CONNECTION_TIMEOUT, 5000).

-define(TENTACLES_DISPATCHER_TIMEOUT, 30000).

%-------------------------------------------------------------------------------
% Types definitions.
%-------------------------------------------------------------------------------

-type base_name() :: atom().
%% Base name of the server.

-type id() :: term().
%% Controller ID.

-type handler() :: pid().
%% Pid of the controller.

-type server_ref() :: Name :: atom()
                    | {Name :: atom(), Node :: node()}
                    | {global, GlobalName :: term()}
                    | {via, Module :: atom(), ViaName :: term()}
                    | pid().
%% Server reference.

-type response() :: {tentacles_controller:response(), tentacles_controller:millisecs()}
                  | {error, timeout}
                  | {error, {unavailable, term()}}
                  | {error, {other, term()}}.
%% Server response.

-type dispatcher_state() :: term().
%% Dispatcher state.

-type event() :: term().
%% Dispatcher event.

%% @doc Internal state of the server.
-record(state, { dict             :: dict()
               , base_name        :: base_name()
               , supervisor       :: module()
               , dispatcher       :: module()
               , controller       :: module()
               , timer            :: reference()
               , dispatcher_state :: dispatcher_state()
               }).

%-------------------------------------------------------------------------------
% Callbacks.
%-------------------------------------------------------------------------------

%% @doc Callback to initialize the controller.
-callback init(BaseName :: base_name(), Args :: list(term())) ->
                  {ok, dispatcher_state()}
                | {ok, dispatcher_state(), tentacles_controller:millisecs()}.

%% @doc Callback to handle timeouts.
-callback handle_timeout(dispatcher_state()) ->
                  {noreply, dispatcher_state()}
                | {stop, tentacles_controller:termination_reason(), dispatcher_state()}.

%% @doc Callback to handle events.
-callback handle_event(event(), dispatcher_state()) ->
                  {noreply, dispatcher_state()}
                | {stop, tentacles_controller:termination_reason(), dispatcher_state()}.

%% @doc Callback to handle termination.
-callback handle_termination(tentacles_controller:termination_reason(), dispatcher_state()) -> term().

%-------------------------------------------------------------------------------
% Public functions.
%-------------------------------------------------------------------------------

-spec start_link(base_name(), [any()]) -> {ok, pid()}
                                        | ignore
                                        | {error, term()}.
%% @doc Starts a tentacles server identified by `Name` and with handlers with
%% max age of `MaxAge`.
start_link(Name, Args) ->
    CompleteArgs = [Name, Args],
    Dispatcher = get_dispatcher_module(Name),
    case gen_server:start_link({local, Dispatcher}, ?MODULE, CompleteArgs, []) of
        {error, {already_started, Pid}} ->
            {ok, Pid};
        Else ->
            Else
    end.

-spec sync_message(base_name(), node(), id(), tentacles_controller:message()) -> response().
%% @doc Sends synchronous `Message` to the `Dispatcher` in a `Node` for the
%%      controller identified by the `Id`.
sync_message(BaseName, Node, Id, Msg) ->
    Dispatcher = get_dispatcher_module(BaseName),
    send_to_server({Dispatcher, Node}, 'sync', {Id, Msg}).

-spec async_message(base_name(), node(), id(), tentacles_controller:message()) -> response().
%% @doc Sends asynchronous `Message` to the `BaseName` dispatcher in a `Node`
%% for the controller identified by the `Id`.
async_message(BaseName, Node, Id, Msg) ->
    Dispatcher = get_dispatcher_module(BaseName),
    send_to_server({Dispatcher, Node}, 'async', {Id, Msg}).

-spec is_alive(base_name(), node(), id()) -> response().
%% @doc Checks if controller `ID` is alive in `Node`.
is_alive(BaseName, Node, Id) ->
    Dispatcher = get_dispatcher_module(BaseName),
    send_to_server({Dispatcher, Node}, 'is_alive', Id).

-spec ping(base_name(), node()) -> response().
%% @doc Pings a dispatcher.
ping(BaseName, Node) ->
    Dispatcher = get_dispatcher_module(BaseName),
    send_to_server({Dispatcher, Node}, 'ping', none).

-spec expire(base_name(), id()) -> ok.
%% @doc Sends expire signal of the controller identified by the `Id` to the
%%      `BaseName` dispatcher. Only allows local calls.
expire(BaseName, Id) ->
    Dispatcher = get_dispatcher_module(BaseName),
    gen_server:cast(Dispatcher, {'expire', Id}).

-spec change_timeout(base_name(), tentacles_controller:millisecs()) -> ok.
%% @doc Changes the dispatchers timeout. Only allows local calls.
change_timeout(BaseName, Timeout) ->
    Dispatcher = get_dispatcher_module(BaseName),
    gen_server:cast(Dispatcher, {'change_timeout', Timeout}).

-spec get_timeout(base_name()) -> response().
%% @doc Gets dispatcher timeout.
get_timeout(BaseName) ->
    Dispatcher = get_dispatcher_module(BaseName),
    send_to_server({Dispatcher, node()}, 'get_timeout', none).

-spec send_event_to_controller(base_name(), node(), id(), event()) -> ok.
%% @doc Sends `Event` to controller identified by `Id`.
send_event_to_controller(BaseName, Node, Id, Event) ->
    Dispatcher = get_dispatcher_module(BaseName),
    gen_server:cast({Dispatcher, Node}, {'event', Id, Event}).

-spec send_event(base_name(), node(), event()) -> ok.
%% @doc Sends `Event` to dispatcher.
send_event(BaseName, Node, Event) ->
    Dispatcher = get_dispatcher_module(BaseName),
    gen_server:cast({Dispatcher, Node}, {'event', Event}).

-spec whois_broadcast(base_name()) -> response().
%% @doc Whois broadcast.
whois_broadcast(BaseName) ->
    Dispatcher = get_dispatcher_module(BaseName),
    send_to_server({Dispatcher, node()}, 'whois_broadcast', none).

-spec stop(base_name(), any()) -> ok.
%% @doc Stops the server.
stop(BaseName, Reason) ->
    Dispatcher = get_dispatcher_module(BaseName),
    gen_server:cast({Dispatcher, node()}, {'event', {'stop', Reason}}).

%-------------------------------------------------------------------------------
% gen_server callbacks definitions.
%-------------------------------------------------------------------------------

% Initializes server state.
init([Name, Args]) ->
    Dispatcher            = get_dispatcher_module(Name),
    Supervisor            = get_controller_supervisor_module(Name),
    Controller            = get_controller_module(Name),
    DispatcherState = case Dispatcher:init(Name, Args) of
        {ok, S} ->
            S;
        {ok, S, Timeout} ->
            set_dispatcher_timeout(Name, Timeout),
            S
    end,
    
    Timer = erlang:send_after(1, self(), 'timeout'),
    
    State = #state{ dict             = dict:new()
                  , base_name        = Name
                  , supervisor       = Supervisor
                  , dispatcher       = Dispatcher
                  , controller       = Controller
                  , timer            = Timer
                  , dispatcher_state = DispatcherState},
    {ok, State}.

% Concurrent requests.
handle_call({'async', {Id, Msg}, Timestamp}, From, State) ->
    case on_time(Timestamp) of
        true ->
            {Handler, NewState} = find_or_create_handler(Id, State),
            tentacles_controller:send_async(Handler, From, Msg),
            {noreply, NewState};
        false ->
            {noreply, State}
    end;

% Serial requests.
handle_call({'sync', {Id, Msg}, Timestamp}, _From, State) ->
    case on_time(Timestamp) of
        true ->
            {Handler, NewState} = find_or_create_handler(Id, State),
            Timeout  = get_connection_timeout(),
            Response = tentacles_controller:send_sync(Handler, Msg, Timeout),
            {reply, Response, NewState};
        false ->
            {noreply, State}
    end;

% Whether a controller is alive or not.
handle_call({'is_alive', Id, Timestamp}, _From, State) ->
    case on_time(Timestamp) of
        true ->
            case find_handler(Id, State) of
                {found, _} ->
                    {reply, yes, State};
                not_found  ->
                    {reply, no, State}
            end;
        false ->
            {noreply, State}
    end;

% Whois request to check which controllers are active.
handle_call({'whois_broadcast', none, Timestamp}, From, State) ->
    case on_time(Timestamp) of
        true  ->
            Dispatcher = State#state.dispatcher,
            Dispatcher ! {'whois_broadcast', From},
            {noreply, State};
        false ->
            {noreply, State}
    end;

% Simple ping.
handle_call({'ping', none, Timestamp}, _From, State) ->
    case on_time(Timestamp) of
        true ->
            {reply, pong, State};
        false ->
            {noreply, State}
    end;

% Gets dispatcher timeout.
handle_call({'get_timeout', none, Timestamp}, _From, State) ->
    case on_time(Timestamp) of
        true ->
            BaseName = State#state.base_name,
            {reply, get_dispatcher_timeout(BaseName), State};
        false ->
            {noreply, State}
    end;

% Other requests.
handle_call(_Msg, _From, State) ->
    {noreply, State}.

% Expiration.
handle_cast({'expire', Id}, State) ->
    NewState = State#state{
        dict = dict:erase(Id, State#state.dict)},
    {noreply, NewState};

% Change Timeout.
handle_cast({'change_timeout', Timeout}, State) ->
    if
        is_integer(Timeout) and (Timeout > 0) ->    
            BaseName = State#state.base_name,
            set_dispatcher_timeout(BaseName, Timeout),
            {noreply, State};
        true ->
            {noreply, State}
    end;

% Dispatcher remote event.
handle_cast({'event', Event}, State) ->
    Dispatcher      = State#state.dispatcher,
    Dispatcher ! Event,
    {noreply, State};

% Controller remote event.
handle_cast({'event', Id, Event}, State) ->
    {Handler, NewState} = find_or_create_handler(Id, State),
    tentacles_controller:send_event(Handler, Event),
    {noreply, NewState};

% Other requests.
handle_cast(_, State) ->
    {noreply, State}.

% Timeout
handle_info('timeout', State) ->
    OldTimer = State#state.timer,
    erlang:cancel_timer(OldTimer),

    Dispatcher      = State#state.dispatcher,
    DispatcherState = State#state.dispatcher_state,
    Result = Dispatcher:handle_timeout(DispatcherState),

    Timeout = get_dispatcher_timeout(State#state.base_name),
    erlang:send_after(Timeout, self(), timeout),
    
    case Result of
        {noreply, NewDispatcherState}      ->
            NewState = State#state{ dispatcher_state = NewDispatcherState},
            {noreply, NewState};
        {stop, Reason, NewDispatcherState} ->
            NewState = State#state{ dispatcher_state = NewDispatcherState},
            {stop, Reason, NewState}
     end;

% Whois
handle_info({'whois_broadcast', From}, State) ->
    spawn(fun() ->
            whois_broadcast(From, State#state.dict)
          end),
    {noreply, State};

%stop
handle_info({'stop', Reason}, State) ->
    {stop, Reason, State};

% Events.
handle_info(Event, State) ->
    Dispatcher      = State#state.dispatcher,
    DispatcherState = State#state.dispatcher_state,
    case Dispatcher:handle_event(Event, DispatcherState) of
        {noreply, NewDispatcherState}      ->
            NewState = State#state{ dispatcher_state = NewDispatcherState},
            {noreply, NewState};
        {stop, Reason, NewDispatcherState} ->
            NewState = State#state{ dispatcher_state = NewDispatcherState},
            {stop, Reason, NewState}
     end.

terminate(Reason, State) ->
    Dispatcher      = State#state.dispatcher,
    DispatcherState = State#state.dispatcher_state,
    Dispatcher:handle_termination(Reason, DispatcherState),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%-------------------------------------------------------------------------------
% Internal functions.
%-------------------------------------------------------------------------------

-spec send_to_server(server_ref(), atom(), term()) -> response().
% @doc Function sends message to a server.
send_to_server(ServerRef, Type, Msg) ->
    Timestamp  = erlang:now(),
    DispMsg    = {Type, Msg, Timestamp},
    Timeout = get_connection_timeout(),
    try
        Reply = gen_server:call(ServerRef, DispMsg, Timeout),
        {Reply, request_time(Timestamp)}
    catch
        exit:{timeout, _}    -> {error, timeout};
        exit:{noproc, Error} -> {error, {unavailable, Error}};
        _:Error              -> {error, {other, Error}}
    end.

-spec request_time(erlang:timestamp()) -> tentacles_controller:millisecs().
% @doc Calculates the request time given a time stamp.
request_time(Timestamp) ->
    timer:now_diff(erlang:now(), Timestamp).

-spec on_time(erlang:timestamp()) -> true | false.
%% @doc If the `Timestamp` is still on time.
on_time(Timestamp) ->
    Timeout = get_connection_timeout(),
    Ms      = request_time(Timestamp),
    if
        Ms >= Timeout ->
            false;
        true ->
            true
    end.

-spec find_or_create_handler(id(), #state{}) -> {handler(), #state{}}.
%% @doc Find or create handler to send requests for the program identified
%%      by `Id`.
find_or_create_handler(Id, #state{ dict       = Id2Handler
                                 , base_name  = BaseName
                                 , supervisor = Supervisor
                                 , controller = Controller} = State) ->
    case find_handler(Id, State) of
        {found, Handler} ->
            {Handler, State};
        not_found        ->
            MaxAge = get_controller_max_age(BaseName),
            Args   = [Controller, BaseName, Id, MaxAge],
            {ok, Handler} = supervisor:start_child(Supervisor, Args),
            {Handler, State#state{
                        dict = dict:store(Id, Handler, Id2Handler)
                      }}
    end.
    
-spec find_handler(id(), #state{}) -> {found, handler()} | not_found.
%% @doc Find the handler
find_handler(Id, #state{dict = Id2Handler}) ->
    case dict:find(Id, Id2Handler) of
        {ok, Handler} ->
            case erlang:is_process_alive(Handler) of
                true  -> {found, Handler};
                false -> not_found
            end;
        _ ->
            not_found
    end.     

%-------------------------------------------------------------------------------
% Configuration functions.
%-------------------------------------------------------------------------------

-spec get_controller_max_age(base_name()) -> tentacles_controller:max_age().
%% @doc Gets controller max age.
get_controller_max_age(Name) ->
    Prop = list_to_atom("tentacles_" ++ atom_to_list(Name) ++ "_controller_max_age"),
    case application:get_env(tentacles, Prop) of
        undefined    -> ?TENTACLES_CONTROLLER_MAX_AGE;
        {ok, MaxAge} -> MaxAge
    end.

-spec get_connection_timeout() -> tentacles_controller:millisecs().
%% @doc Gets connection timeout time.
get_connection_timeout() ->
    case application:get_env(tentacles, tentacles_connection_timeout) of
        undefined     -> ?TENTACLES_CONNECTION_TIMEOUT;
        {ok, Timeout} -> Timeout
    end.

-spec get_dispatcher_timeout(base_name()) -> tentacles_controller:millisecs().
%% @doc Gets dispatcher timeout.
get_dispatcher_timeout(Name) ->
    Prop = list_to_atom("tentacles_" ++ atom_to_list(Name) ++ "_dispatcher_timeout"),
    case application:get_env(tentacles, Prop) of
        undefined     -> ?TENTACLES_DISPATCHER_TIMEOUT;
        {ok, Timeout} -> Timeout
    end.

-spec set_dispatcher_timeout(base_name(), tentacles_controller:millisecs()) -> ok.
%% @doc Sets dispatcher `Name` timeout `Timeout`.
set_dispatcher_timeout(Name, Timeout) ->
    Prop = list_to_atom("tentacles_" ++ atom_to_list(Name) ++ "_dispatcher_timeout"),
    application:set_env(tentacles, Prop, Timeout).

-spec get_dispatcher_module(base_name()) -> module().
%% @doc Get dispatcher module name.
get_dispatcher_module(Name) ->
    list_to_atom("tentacles_" ++ atom_to_list(Name) ++ "_dispatcher").

-spec get_controller_supervisor_module(base_name()) -> module().
%% @doc Get controller supervisor module name.
get_controller_supervisor_module(Name) ->
    list_to_atom("tentacles_" ++ atom_to_list(Name) ++ "_controller_sup").

-spec get_controller_module(base_name()) -> module().
%% @doc Get controller module name.
get_controller_module(Name) ->
    list_to_atom("tentacles_" ++ atom_to_list(Name) ++ "_controller").

-spec whois_broadcast(From :: {pid(), term()}, Dict :: dict()) -> term().
%% @doc Whois broadcast.
whois_broadcast(From, Dict) ->
    Ids = proplists:get_keys(dict:to_list(Dict)),
    gen_server:reply(From, {ok, Ids}).
