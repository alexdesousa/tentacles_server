-module(tentacles_proper_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DISPATCHER, tentacles_dispatcher).
-define(SERVER, tentacles_server_sup).

server_test() ->
    ?SERVER:start_link(test, []),
    test().

test() ->
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_sync_message(), [{to_file, user}])),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_async_message(), [{to_file, user}])),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_is_alive(), [{to_file, user}])),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_expire(), [{to_file, user}])),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_timeout(), [{to_file, user}])),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_send_event_to_controller(), [{to_file, user}])),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_send_event(), [{to_file, user}])),
    ?assertEqual(true, ?MODULE:prop_stop()).

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

prop_stop() ->
    ok =:= ?DISPATCHER:stop(test, normal).

id() -> atom().

good_timeout() -> range(30000, 40000).
