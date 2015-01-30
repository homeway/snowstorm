%% -*- mode: nitrogen -*-
-module(ss_world_test).
-include_lib("eunit/include/eunit.hrl").

-define(world, to_test_ss_world).
to_test() ->
    %% 启动world
    ss_world:start2(?world),
    ss_world:destroy2(?world),
    ?assertEqual([], ss_world:all2(?world)),

    %% regss ss_user1
    P1 = test_user1,
    P2 = test_user2,
    ss_world:reg_server2(?world, P1, ss_user2),
    ?assertEqual(true, is_pid(ss_world:find2(?world, P1))),
    ?assertEqual(not_reg, ss_world:find2(?world, P2)),

    %% regss ss_user2
    ss_world:reg_server2(?world, P2, ss_user2),
    ?assertEqual(true, is_pid(ss_world:find2(?world, P2))),

    %% return all
    ?assertMatch(2, length(ss_world:all2(?world))),

    %% unreg ss_user2
    ss_world:unreg2(?world, P2),
    ?assertEqual(not_reg, ss_world:find2(?world, P2)),

    %% get process info
    ?assertMatch([{current_function,_}|_], ss_world:info2(?world, P1)),

    %% destroy the world to clean all process
    ss_world:destroy2(?world),
    ?assertEqual([], ss_world:all2(?world)),

    %% stop the world
    ss_world:stop2(?world).
