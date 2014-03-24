-module(tentacles_controller_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(BaseName) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [BaseName]).

init([BaseName]) ->
    ControllerName = tentacles_dispatcher:get_controller_module(BaseName),
    Controller     = { ControllerName
                     , {ControllerName, start_link, []}
                     , temporary, 2000, worker, [ControllerName]},
    
    RestartStrategy = {simple_one_for_one, 0, 10},
    Children = [Controller],
    
    {ok, {RestartStrategy, Children}}.
