-module(tentacles_test_dispatcher).

-behaviour(tentacles_dispatcher).

-export([start_link/1]).

% Callbacks.
-export([init/2, handle_timeout/1, handle_event/2, handle_termination/2]).

-record(state, {base_name :: tentacles_dispatcher:base_name()}).

start_link(Name) ->
    tentacles_dispatcher:start_link(Name, []).

init(Name, []) ->
    State = #state{base_name = Name},
    {ok, State}.

handle_timeout(State) ->
    {noreply, State}.

handle_event(_Event, State) ->
    {noreply, State}.

handle_termination(_Reason, _State) ->
    ok.
