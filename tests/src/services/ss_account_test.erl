%% -*- mode: nitrogen -*-
-module(ss_account_test).
-include_lib("eunit/include/eunit.hrl").
-define(world, to_test_ss_account).
-define(offline, {service, offline}).

to_test() ->
    %% 启动world
    ss_world:start2(?world),
    ss_world:destroy2(?world),
    ?assertEqual([], ss_world:all2(?world)),

    %% resource server: {res, account}
    Res = {res, account},
    ss_world:reg_server2(?world, Res, ss_account, [#{res=>ss_account2}]),
    ?assertEqual(true, is_pid(ss_world:find2(?world, Res))),

    %% service server: {service, offline}
    ss_world:reg_server2(?world, ?offline, ss_offline, [#{res=>ss_offline}]),
    ?assertEqual(true, is_pid(ss_world:find2(?world, ?offline))),
    ?assertEqual(ok, ss_world:call2(?world, ?offline, drop)),
    erlang:display("world info >>>>>>>>>"),
    erlang:display(ss_world:all2(?world)),
    erlang:display("------------------------"),

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
    Adi = {account, "adi"},
    M2 = ss_world:call2(?world, Res, [model, all]),
    D2 = #{account => "adi", password => "123456", contacts => [{"yifan", #{rel=>double}}]},
    {ok, Id2} = ss_world:call2(?world, Res, [create, D2, M2]),
    ss_world:call2(?world, Res, [find, Id2]),
    ss_world:reg_server2(?world, Adi, ss_account, [#{res=>ss_account2}]),
    ss_world:call2(?world, Adi, [connect, self()]),
    ?assertMatch(not_login, ss_world:call2(?world, Adi, status)),
    ?assertMatch(ok, ss_world:call2(?world, Adi, [login, "adi", "123456"])),
    ?assertMatch("online", ss_world:call2(?world, Adi, status)),

    %% 添加订阅列表到"yifan"
    Yifan = {account, "yifan"},
    M3 = ss_world:call2(?world, Res, [model, contacts]),
    D3 = #{contacts => [{"adi", #{rel=>double}},
                        {"homeway", #{rel=>single}}]},
    ?assertMatch(ok, ss_world:call2(?world, Res, [update, Id1, D3, M3])),

    %% 出席
    ss_world:reg_server2(?world, Yifan, ss_account, [#{res=>ss_account2}]),
    ss_world:call2(?world, Yifan, [connect, self()]),
    ?assertMatch(not_login, ss_world:call2(?world, Yifan, contacts)),
    ?assertMatch(ok, ss_world:call2(?world, Yifan, [login, "yifan", "123456"])),
    ss_world:all2(?world),

    %% 收取双方出席通知
    ?assertMatch({online, "yifan"}, got_msg()),
    ?assertMatch({online, "adi"}, got_msg()),

    %% 检查双方联系人出席情况
    Contacts1 = [{"yifan", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts1, ss_world:call2(?world, Adi, contacts)),
    Contacts2 = [{"adi", #{rel=>double, status=>"online"}},
                 {"homeway", #{rel=>single, status=>"offline"}}],
    ?assertEqual(Contacts2, ss_world:call2(?world, Yifan, contacts)),

    %% 添加联系人
    %%
    Homeway = {account, "homeway"},
    D4 = #{account => "homeway", password => "123456"},
    ?assertMatch({ok, _}, ss_world:call2(?world, Res, [create, D4, M1])),
    ss_world:reg_server2(?world, Homeway, ss_account, [#{res=>ss_account2}]),
    ?assertMatch(ok, ss_world:call2(?world, Homeway, [login, "homeway", "123456"])),

    %% homeway添加adi为联系人，双方在线
    erlang:display("homeway add adi >> ........."),
    ?assertEqual(ok, ss_world:call2(?world, Homeway, [invite, "adi"])),
    clear_msg(),
    Contacts4 = [{"adi", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts4, ss_world:call2(?world, Homeway, contacts)),
    Contacts5 = [{"homeway", #{rel=>double, status=>"online"}},
                 {"yifan", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts5, ss_world:call2(?world, Adi, contacts)),

    %% adi添加wenxuan为联系人，但wenxuan不在线
    Wenxuan = {account, "wenxuan"},
    D6 = #{account => "wenxuan", password => "123456"},
    ?assertMatch({ok, _}, ss_world:call2(?world, Res, [create, D6, M1])),
    ?assertEqual(ok, ss_world:call2(?world, Adi, [invite, "wenxuan"])),
    clear_msg(),
    %% wenxuan登录前, wenxuan为离线状态，关系为single
    Contacts6 = [{"wenxuan", #{rel=>single, status=>"offline"}},
                 {"homeway", #{rel=>double, status=>"online"}},
                 {"yifan", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts6, ss_world:call2(?world, Adi, contacts)),
    %% wenxuan登录后
    ss_world:reg_server2(?world, Wenxuan, ss_account, [#{res=>ss_account2}]),
    ss_world:call2(?world, Wenxuan, [connect, self()]),
    ?assertMatch(ok, ss_world:call2(?world, Wenxuan, [login, "wenxuan", "123456"])),
    clear_msg(),
    erlang:display("wenxuan login ....."),
    %% wenxuan收到离线邀请, 更新联系人
    Contacts7 = [{"adi", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts7, ss_world:call2(?world, Wenxuan, contacts)),
    %% adi也更新联系人信息
    Contacts8 = [{"wenxuan", #{rel=>double, status=>"online"}},
                 {"homeway", #{rel=>double, status=>"online"}},
                 {"yifan",   #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts8, ss_world:call2(?world, Adi, contacts)),

    %% stop the world
    ?assertEqual(ok, ss_world:call2(?world, Res, drop)),
    ?assertEqual(ok, ss_world:call2(?world, ?offline, drop)),
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