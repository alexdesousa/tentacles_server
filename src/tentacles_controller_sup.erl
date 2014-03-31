-module(tentacles_controller_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(BaseName) ->
    ServerName = list_to_atom("tentacles_" ++ atom_to_list(BaseName) ++ "_controller_sup"),
    supervisor:start_link({local, ServerName}, ?MODULE, [BaseName]).

init([BaseName]) ->
    ControllerName = tentacles_dispatcher:get_controller_module(BaseName),
    Controller     = { ControllerName
                     , {ControllerName, start_link, []}
                     , temporary, 2000, worker, [ControllerName]},
    
    RestartStrategy = {simple_one_for_one, 0, 10},
    Children = [Controller],
    
    {ok, {RestartStrategy, Children}}.
