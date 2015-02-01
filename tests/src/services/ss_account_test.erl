%% -*- mode: nitrogen -*-
-module(ss_account_test).
-include_lib("eunit/include/eunit.hrl").
-define(world, to_test_ss_account).

to_test() ->
    %% 启动world
    ss_world:start2(?world),
    ss_world:destroy2(?world),
    ?assertEqual([], ss_world:all2(?world)),

    %% reg_server ss_account
    Res = {res, account},
    ss_world:reg_server2(?world, Res, ss_account, [#{res=>ss_account2}]),
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

    %% 首先启动用户"adi"
    P2 = {account, "adi"},
    M2 = ss_world:call2(?world, Res, [model, all]),
    D2 = #{account => "adi", password => "123456", contacts => [{"yifan", #{rel=>double}}]},
    {ok, Id2} = ss_world:call2(?world, Res, [create, D2, M2]),
    erlang:display(ss_world:call2(?world, Res, [find, Id2])),
    ss_world:reg_server2(?world, P2, ss_account, [#{res=>ss_account2}]),
    ss_world:call2(?world, P2, [connect, self()]),
    ?assertMatch(not_login, ss_world:call2(?world, P2, status)),
    ?assertMatch(ok, ss_world:call2(?world, P2, [login, "adi", "123456"])),
    ?assertMatch("online", ss_world:call2(?world, P2, status)),

    %% 添加订阅列表到"yifan"
    P3 = {account, "yifan"},
    M3 = ss_world:call2(?world, Res, [model, contacts]),
    D3 = #{contacts => [{"adi", #{rel=>double}}, {"homeway", #{rel=>single}}]},
    ?assertMatch(ok, ss_world:call2(?world, Res, [update, Id1, D3, M3])),

    %% 出席
    ss_world:reg_server2(?world, P3, ss_account, [#{res=>ss_account2}]),
    ss_world:call2(?world, P3, [connect, self()]),
    ?assertMatch(not_login, ss_world:call2(?world, P3, contacts)),
    ?assertMatch(ok, ss_world:call2(?world, P3, [login, "yifan", "123456"])),
    ss_world:all2(?world),

    %% 收取双方出席通知
    ?assertMatch({online, "yifan"}, got_msg()),
    ?assertMatch({online, "adi"}, got_msg()),

    %% 检查双方联系人出席情况
    Contacts1 = [{"yifan", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts1, ss_world:call2(?world, P2, contacts)),
    Contacts2 = [{"adi", #{rel=>double, status=>"online"}}, {"homeway", #{rel=>single, status=>"offline"}}],
    ?assertEqual(Contacts2, ss_world:call2(?world, P3, contacts)),

    %% 添加联系人
    %%
    P4 = {account, "homeway"},
    D4 = #{account => "homeway", password => "123456"},
    ?assertMatch({ok, _}, ss_world:call2(?world, Res, [create, D4, M1])),
    ss_world:reg_server2(?world, P4, ss_account, [#{res=>ss_account2}]),
    ?assertMatch(ok, ss_world:call2(?world, P4, [login, "homeway", "123456"])),
    %% 设为自己的联系人
    erlang:display("set homeway to contact adi"),
    erlang:display(ss_world:call2(?world, P4, info)),
    ?assertEqual(ok, ss_world:call2(?world, P4, [contact, "adi"])),
    erlang:display(ss_world:call2(?world, P4, info)),
    Contacts4 = [{"adi", #{rel=>single, status=>"offline"}}],
    ?assertEqual(Contacts4, ss_world:call2(?world, P4, contacts)),
    %% 邀请对方加为联系人
    erlang:display("invite adi to contact homeway"),
    erlang:display(ss_world:call2(?world, P2, info)),
    erlang:display(ss_world:call2(?world, Res, [find, Id2])),
    ss_world:send2(?world, P2, [invite, "homeway"]),
    clear_msg(),
    erlang:display(ss_world:call2(?world, P2, info)),
    erlang:display(ss_world:call2(?world, Res, [find, Id2])),
    Contacts5 = [{"homeway", #{rel=>double, status=>"online"}}, {"yifan", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts5, ss_world:call2(?world, P2, contacts)),

    %% stop the world
    ?assertEqual(ok, ss_world:call2(?world, Res, drop)),
    ss_world:stop2(?world).

%% 如果没有任何消息就停顿100毫秒再确定消息已清除
clear_msg() ->
    receive _ -> clear_msg()
    after 100 -> ok
    end.

got_msg() ->
    receive Msg -> Msg
    after 200 -> nothing
    end.