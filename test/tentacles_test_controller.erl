-module(tentacles_test_controller).

-behaviour(tentacles_controller).

-export([start_link/4]).

% Callbacks.
-export([init/1, handle_message/2, handle_timeout/1, handle_event/2,
         handle_termination/2]).

-record(state, { base_name :: tentacles_dispatcher:base_name()
               , id        :: tentacles_dispatcher:id()
               , max_age   :: tentacles_controller:max_age()}).

start_link(BaseName, Controller, Id, MaxAge) ->
    tentacles_controller:start_link(Controller, [BaseName, Id, MaxAge]).

init([BaseName, Id, MaxAge]) ->
    State = #state{ base_name = BaseName
                  , id        = Id},
    {ok, State, MaxAge}.

handle_message(ping, State) ->
    {reply, pong, State};
handle_message({change_timeout, Timeout}, State) ->
    BaseName = State#state.base_name,
    tentacles_dispatcher:change_dispatcher_timeout(BaseName, Timeout),
    {reply, ok, State};
handle_message(_Any, State) ->
    {noreply, State}.

handle_timeout(State) ->
    BaseName = State#state.base_name,
    Id       = State#state.id,
    tentacles_dispatcher:expire(BaseName, Id),
    {stop, normal, State}.

handle_event(expire, State) ->
    BaseName = State#state.base_name,
    Id       = State#state.id,
    tentacles_dispatcher:expire(BaseName, Id),
    {stop, normal, State}.

handle_termination(_Reason, _State) ->
    ok.
