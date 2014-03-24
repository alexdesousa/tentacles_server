-module(tentacles_dispatcher).

-behaviour(gen_server).

-export([start_link/2, sync_message/4, async_message/4, expire/2]).

-export([get_dispatcher_module/1, get_controller_module/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(TENTACLES_CONTROLLER_MAX_AGE, 30000).

-define(TENTACLES_CONNECTION_TIMEOUT, 5000).

-define(TENTACLES_DISPATCHER_TIMEOUT, 30000).

%-------------------------------------------------------------------------------
% Types definitions.
%-------------------------------------------------------------------------------

-type module() :: atom().
%% Module.

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

-type response() :: {tentacles_controller:response(), tentacles_controller:millisecs()}.
%% Server response.

-type dispatcher_state() :: term().
%% Dispatcher state.

-type event() :: term().
%% Dispatcher event.

%% @doc Internal state of the server.
-record(state, { dict             :: dict()
               , supervisor       :: module()
               , dispatcher       :: module()
               , controller       :: module()
               , dispatcher_state :: dispatcher_state()
               }).

%-------------------------------------------------------------------------------
% Callbacks.
%-------------------------------------------------------------------------------

%% @doc Callback to initialize the controller.
-callback init(Args :: list(term())) ->
                {ok, dispatcher_state(), tentacles_controller:millisecs()}.

%% @doc Callback to handle timeouts.
-callback handle_timeout(dispatcher_state()) ->
                  {noreply, dispatcher_state()}
                | {stop, tentacles_controller:termination_reason(), dispatcher_state()}.

%% @doc Callback to handle events.
-callback handle_event(event(), dispatcher_state()) ->
                  {noreply, dispatcher_state()}
                | {stop, tentacles_controller:termination_reason(), dispatcher_state()}.

%% @doc Callback to handle termination.
-callback handle_termination(tentacles_controller:termination_reason(), dispatcher_state) -> term().

%-------------------------------------------------------------------------------
% Public functions.
%-------------------------------------------------------------------------------

-spec start_link(base_name(), [any()]) -> {ok, pid()}
                                        | ignore
                                        | {error, term()}.
%% @doc Starts a tentacles server identified by `Name` and with handlers with
%% max age of `MaxAge`.
start_link(Name, Args) ->
    Args = [Name, Args],
    case gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []) of
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
    send_to_server({Dispatcher, Node}, sync, Id, Msg).

-spec async_message(base_name(), node(), id(), tentacles_controller:message()) -> response().
%% @doc Sends asynchronous `Message` to the `BaseName` dispatcher in a `Node`
%% for the controller identified by the `Id`.
async_message(BaseName, Node, Id, Msg) ->
    Dispatcher = get_dispatcher_module(BaseName),
    send_to_server({Dispatcher, Node}, async, Id, Msg).

-spec expire(base_name(), id()) -> ok.
%% @doc Sends expire signal of the controller identified by the `Id` to the
%%      `BaseName` dispatcher. Only allows local calls.
expire(BaseName, Id) ->
    Dispatcher = get_dispatcher_module(BaseName),
    gen_server:handle_cast(Dispatcher, {expire, Id}).

%-------------------------------------------------------------------------------
% gen_server callbacks definitions.
%-------------------------------------------------------------------------------

% Initializes server state.
init([Name, Args]) ->
    Dispatcher      = get_dispatcher_module(Name),
    Supervisor      = get_controller_supervisor_module(Name),
    Controller      = get_controller_module(Name),
    DispatcherState = Dispatcher:init(Args),
    State = #state{ dict             = dict:new()
                  , supervisor       = Supervisor
                  , dispatcher       = Dispatcher
                  , controller       = Controller
                  , dispatcher_state = DispatcherState},
    {ok, State, get_dispatcher_timeout()}.

% Concurrent requests.
handle_call({async, {Id, Msg}, Timestamp}, From, State) ->
    case on_time(Timestamp) of
        true ->
            {Handler, NewState} = find_or_create_handler(Id, State),
            tentacles_controller:send_async(Handler, From, Msg),
            {noreply, NewState, get_dispatcher_timeout()};
        false ->
            {noreply, State, get_dispatcher_timeout()}
    end;

% Serial requests.
handle_call({sync, {Id, Msg}, Timestamp}, _From, State) ->
    case on_time(Timestamp) of
        true ->
            {Handler, NewState} = find_or_create_handler(Id, State),
            Timeout  = get_connection_timeout(),
            Response = tentacles_controller:send_sync(Handler, Msg, Timeout),
            {reply, Response, NewState, get_dispatcher_timeout()};
        false ->
            {noreply, State, get_dispatcher_timeout()}
    end;

% Other requests.
handle_call(_Msg, _From, State) ->
    {noreply, State, get_dispatcher_timeout()}.

% Expiration.
handle_cast({expire, Id}, State) ->
    NewState = State#state{
        dict = dict:erase(Id, State#state.dict)},
    {noreply, NewState};

% Other requests.
handle_cast(_, State) ->
    {noreply, State}.

% Timeout
handle_info(timeout, State) ->
    Dispatcher      = State#state.dispatcher,
    DispatcherState = State#state.dispatcher_state,
    Dispatcher:handle_timeout(DispatcherState),
    {noreply, State};

% Events.
handle_info(Event, State) ->
    Dispatcher      = State#state.dispatcher,
    DispatcherState = State#state.dispatcher_state,
    Dispatcher:handle_event(Event, DispatcherState),
    {noreply, State}.

terminate(Reason, State) ->
    Dispatcher      = State#state.dispatcher,
    DispatcherState = State#state.dispatcher_state,
    Dispatcher:handle_terminate(Reason, DispatcherState),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%-------------------------------------------------------------------------------
% Internal functions.
%-------------------------------------------------------------------------------

-spec send_to_server(server_ref(), sync | async, id(), tentecles_controller:message()) -> response().
% @doc Function sends message to a server.
send_to_server(ServerRef, Type, Id, Msg) ->
    Timestamp  = erlang:now(),
    DispMsg    = {Type, {Id, Msg}, Timestamp},
    Timeout = get_connection_timeout(),
    try
        Reply = gen_server:call(ServerRef, DispMsg, Timeout),
        {Reply, request_time(Timestamp)}
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:{noproc, _}  -> {error, unavailable};
        _:_               -> {error, unknown}
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
                                 , supervisor = Supervisor
                                 , dispatcher = Dispatcher
                                 , controller = Controller} = State) ->
    case dict:find(Id, Id2Handler) of
        {ok, Handler} ->
            {Handler, State};
        _ ->
            MaxAge = get_controller_max_age(),
            Args   = [Dispatcher, Controller, Id, MaxAge],
            {ok, Handler} = supervisor:start_child(Supervisor, Args),
            {Handler, State#state{
                        dict = dict:store(Id, Handler, Id2Handler)
                      }}
    end.

%-------------------------------------------------------------------------------
% Configuration functions.
%-------------------------------------------------------------------------------

-spec get_controller_max_age() -> tentacles_controller:max_age().
%% @doc Gets controller max age.
get_controller_max_age() ->
    case application:get_env(tentacles, tentacles_controller_max_age) of
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

-spec get_dispatcher_timeout() -> tentacles_controller:millisecs().
%% @doc Gets dispatcher timeout.
get_dispatcher_timeout() ->
    case application:get_env(tentacles, tentacles_dispatcher_timeout) of
        undefined     -> ?TENTACLES_DISPATCHER_TIMEOUT;
        {ok, Timeout} -> Timeout
    end.

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
