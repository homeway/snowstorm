%% -*- mode: nitrogen -*-
-module(ss_user_test).
-include_lib("eunit/include/eunit.hrl").
-define(world, to_test_ss_user).

to_test() ->
    %% 启动world
    ss_world:start2(?world),
    ss_world:destroy2(?world),
    ?assertEqual([], ss_world:all2(?world)),

    %% reg_server ss_user
    P1 = test_user1,
    ss_world:reg_server2(?world, P1, ss_user),
    ?assertEqual(true, is_pid(ss_world:find2(?world, P1))),

    %% clear data
    ?assertEqual(ok, ss_world:call2(?world, P1, drop)),
    ?assertEqual([], ss_world:call2(?world, P1, all)),

    %% create an account with yifan
    M1 = ss_world:call2(?world, P1, [model, password]),
    D1 = #{<<"account">> => "yifan", <<"password">> => "123456"},
    {ok, Id1} = ss_world:call2(?world, P1, [create, D1, M1]),
    ?assertMatch({error, _}, ss_world:call2(?world, P1, [create, D1, M1])),
    

    %% stop the world
    ?assertEqual(ok, ss_world:call2(?world, P1, drop)),
    ss_world:stop2(?world).
