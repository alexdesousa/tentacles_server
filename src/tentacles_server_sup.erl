-module(tentacles_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% @doc Starts a tentacles server with base name `BaseName` and arguments `Args`.
%%      These `Args` will be passed to the dispatcher as arguments for the
%%      tentacles_<BaseName>_dispatcher:init/2 function.
start_link(BaseName, Args) ->
    ServerName = list_to_atom("tentacles_" ++ atom_to_list(BaseName) ++ "_server_sup"),
    supervisor:start_link({local, ServerName}, ?MODULE, [BaseName, Args]).

%% @doc Initializes the dispatcher and the controller supervisor.
init([BaseName, Args]) ->
    DispatcherName = tentacles_dispatcher:get_dispatcher_module(BaseName),
    Dispatcher     = { DispatcherName
                     , {DispatcherName, start_link, [BaseName | Args]}
                     , permanent, 2000, worker, [DispatcherName]},

    ControllerSupName = list_to_atom("tentacles_" ++ atom_to_list(BaseName) ++ "_controller_sup"),
    ControllerSup     = { ControllerSupName
                        , {tentacles_controller_sup, start_link, [BaseName]}
                        , permanent, 2000, supervisor, [tentacles_controller_sup]},
    
    RestartStrategy = {one_for_one, 10, 10},
    Children        = [ControllerSup, Dispatcher],
    
    {ok, {RestartStrategy, Children}}.
