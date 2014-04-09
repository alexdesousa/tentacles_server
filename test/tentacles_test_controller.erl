-module(tentacles_test_controller).

-behaviour(tentacles_controller).

% Callbacks.
-export([init/2, handle_message/2, handle_timeout/1, handle_event/2,
         handle_termination/2]).

-record(state, { base_name :: tentacles_dispatcher:base_name()
               , id        :: tentacles_dispatcher:id()}).

init(BaseName, Id) ->
    State = #state{ base_name = BaseName
                  , id        = Id},
    {ok, State}.

handle_message(ping, State) ->
    {reply, pong, State};
handle_message(_Any, State) ->
    {noreply, State}.

handle_timeout(State) ->
    {noreply, State}.

handle_event(expire, State) ->
    BaseName = State#state.base_name,
    Id       = State#state.id,
    tentacles_dispatcher:expire(BaseName, Id),
    {stop, normal, State}.

handle_termination(_Reason, _State) ->
    ok.
