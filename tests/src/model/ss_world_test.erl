%% -*- mode: nitrogen -*-
-module(ss_world_test).
-include_lib("eunit/include/eunit.hrl").

-define(world, to_test_ss_world).
to_test() ->
    %% 启动world
    W = ss:world(to_test_ss_world),
    W:start(),
    W:destroy(),
    ?assertEqual([], W:all()),

    %% regss ss_user1
    P1 = test_user1,
    P2 = test_user2,
    W:reg_server(P1, ss_user2),
    ?assertEqual(true, is_pid(W:find(P1))),
    ?assertEqual(not_reg, W:find(P2)),

    %% regss ss_user2
    W:reg_server(P2, ss_user2),
    ?assertEqual(true, is_pid(W:find(P2))),

    %% return all
    ?assertMatch(2, length(W:all())),

    %% unreg ss_user2
    W:unreg(P2),
    ?assertEqual(not_reg, W:find(P2)),

    %% get process info
    ?assertMatch([{current_function,_}|_], W:info(P1)),

    %% call wolrd in ss_server
    ?assertMatch([{current_function,_}|_], W:call(P1, [known_world, P1])),

    %% destroy the world to clean all process
    W:destroy(),
    ?assertEqual([], W:all()),

    %% stop the world
    W:stop().
