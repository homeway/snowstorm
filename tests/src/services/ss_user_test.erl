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
    Res = {res, user},
    ss_world:reg_server2(?world, Res, ss_user, [#{res=>ss_user2}]),
    ?assertEqual(true, is_pid(ss_world:find2(?world, Res))),

    %% clear data
    ?assertEqual(ok, ss_world:call2(?world, Res, drop)),
    ?assertEqual([], ss_world:call2(?world, Res, all)),

    %% create an account with yifan
    M1 = ss_world:call2(?world, Res, [model, password]),
    D1 = #{account => "yifan", password => "123456"},
    {ok, Id1} = ss_world:call2(?world, Res, [create, D1, M1]),

    %% account must be uniq
    ?assertMatch({error, _}, ss_world:call2(?world, Res, [create, D1, M1])),

    %% 出席通知
    %%
    clear_msg(),

    %% 首先启动用户adi
    P2 = {user, "adi"},
    D2 = #{account => "adi", password => "123456"},
    {ok, _} = ss_world:call2(?world, Res, [create, D2, M1]),
    ss_world:reg_server2(?world, P2, ss_user, [#{res=>ss_user2}]),
    erlang:display(ss_world:call2(?world, P2, [connect, self()])),

    %% 添加订阅列表
    P3 = {user, "yifan"},
    M3 = ss_world:call2(?world, Res, [model, pub_to]),
    D3 = #{pub_to => [{user, "adi"}, {user, "homeway"}]},
    ?assertMatch(ok, ss_world:call2(?world, Res, [update, Id1, D3, M3])),

    %% 出席
    ss_world:reg_server2(?world, P3, ss_user, [#{res=>ss_user2}]),
    ?assertMatch(ok, ss_world:call2(?world, P3, [login, "yifan", "123456"])),
    erlang:display(ss_world:all2(?world)),

    %% 收取出席通知
    ?assertMatch({online, "yifan"}, got_msg()),

    %% 联系人

    %% 订阅出席通知

    %% 接收出席通知

    %% 询问出席状态

    %% stop the world
    ?assertEqual(ok, ss_world:call2(?world, Res, drop)),
    ss_world:stop2(?world).

clear_msg() ->
    receive _ -> clear_msg()
    after 1 -> ok
    end.

got_msg() ->
    receive Msg -> Msg
    after 200 -> nothing
    end.