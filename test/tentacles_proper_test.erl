-module(tentacles_proper_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DISPATCHER, tentacles_dispatcher).
-define(SERVER, tentacles_server_sup).
-define(NUMBER_OF_TESTS, 100).

server_test() ->
    ?SERVER:start_link(test, []),
    test().

test() ->
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_sync_message(), [{to_file, user}])),
    message_time(sync_message, ?NUMBER_OF_TESTS),
    
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_async_message(), [{to_file, user}])),
    message_time(async_message, ?NUMBER_OF_TESTS),

    ?assertEqual(true, proper:quickcheck(?MODULE:prop_concurrent_message(), [{to_file, user}])),
    message_time(concurrent_message, ?NUMBER_OF_TESTS),

    ?assertEqual(true, proper:quickcheck(?MODULE:prop_is_alive(), [{to_file, user}])),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_ping(), [{to_file, user}])),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_expire(), [{to_file, user}])),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_timeout(), [{to_file, user}])),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_send_event_to_controller(), [{to_file, user}])),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_send_event(), [{to_file, user}])),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_whois_broadcast(), [{to_file, user}])),
    ?assertEqual(true, ?MODULE:prop_stop()).

message_time(Function, Tests) ->
    message_time(Function, 0, Tests, Tests).

message_time(Function, Acc, N, 0) ->
    ?debugFmt("~p/4 average time: ~p μs", [Function, Acc/N]);
message_time(Function, Acc, N, M) ->
    {ok, Id} = proper_gen:pick(id()),
    F = fun() ->
        ?DISPATCHER:Function(test, node(), Id, ping)
    end,
    case F() of
        {pong, Ms} ->
            message_time(Function, Acc + Ms, N, M - 1);
        _ ->
            message_time(Function, Acc, N - 1, M - 1)
    end.



prop_sync_message() ->
    ?FORALL(Id, id(),
        begin
            case ?DISPATCHER:sync_message(test, node(), Id, ping) of
                {pong, _} ->
                    true;
                _ ->
                    false
            end
        end
    ).

prop_async_message() ->
    ?FORALL(Id, id(),
        begin
            case ?DISPATCHER:async_message(test, node(), Id, ping) of
                {pong, _} ->
                    true;
                _ ->
                    false
            end
        end
    ).

prop_concurrent_message() ->
    ?FORALL(Id, id(),
        begin
            case ?DISPATCHER:concurrent_message(test, node(), Id, ping) of
                {pong, _} ->
                    true;
                _ ->
                    false
            end
        end
    ).

prop_is_alive() ->
    ?FORALL(Id, id(),
        begin
            ?DISPATCHER:sync_message(test, node(), Id, ping),
            case ?DISPATCHER:is_alive(test, node(), Id) of
                {yes, _} ->
                    true;
                _ ->
                    false
            end
        end
    ).

prop_ping() ->
    case ?DISPATCHER:ping(test, node()) of
        {pong, _} ->
            true;
        _ ->
            false
    end.

prop_expire() ->
    ?FORALL(Id, id(),
        begin
            ?DISPATCHER:sync_message(test, node(), Id, ping),
            ?DISPATCHER:expire(test, Id),
            case ?DISPATCHER:is_alive(test, node(), Id) of
                {no, _} ->
                    true;
                _ ->
                    false
            end
        end
    ).

prop_timeout() ->
    ?FORALL(Timeout, good_timeout(),
        begin
            ?DISPATCHER:change_timeout(test, Timeout),
            {NewTimeout, _} = ?DISPATCHER:get_timeout(test),
            NewTimeout =:= Timeout
        end
    ).

prop_send_event_to_controller() ->
    ?FORALL(Id, id(),
        begin
            ?DISPATCHER:sync_message(test, node(), Id, ping),
            ?DISPATCHER:send_event_to_controller(test, node(), Id, expire),
            case ?DISPATCHER:is_alive(test, node(), Id) of
                {no, _} ->
                    true;
                {yes,_} ->
                    true;
                _ ->
                    false
            end
        end
    ).

prop_send_event() ->
    ok =:= ?DISPATCHER:send_event(test, node(), event).

prop_whois_broadcast() ->
    ?FORALL(Ids, ids(),
        begin
            % Clear old.
            case ?DISPATCHER:whois_broadcast(test) of
                {{ok, Old}, _} ->
                    lists:map(fun(Id) -> ?DISPATCHER:expire(test, Id) end, Old);
                _ ->
                    nothing
            end,
        
            % Create new.
            Filtered = lists:usort(
                lists:foldl(fun(Id, Acc) ->
                    case ?DISPATCHER:sync_message(test, node(), Id, ping) of
                        {pong, _} ->
                            [Id | Acc];
                        _ ->
                            Acc
                    end
                end, [], Ids)),
            
            % Check.
            Result = case ?DISPATCHER:whois_broadcast(test) of
                {{ok, Active}, _} ->
                    Filtered =:= lists:sort(Active);
                _ ->
                    false
            end,
            
            % Clean.
            lists:map(fun(Id) -> ?DISPATCHER:expire(test, Id) end, Filtered),
            
            Result
        end
    ).

prop_stop() ->
    ok =:= ?DISPATCHER:stop(test, normal).

id() -> integer().

ids() ->
    ?SUCHTHAT(Ids, list(id()), length(Ids) >  0).

good_timeout() -> range(30000, 40000).
