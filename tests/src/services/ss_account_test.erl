%% -*- mode: nitrogen -*-
-module(ss_account_test).
-include_lib("eunit/include/eunit.hrl").
-define(world, to_test_ss_account).
-define(offline, {service, offline}).

to_test() ->
    %% 启动world
    W = ss:module({ss_world, ?world}),
    W:start2(),
    W:destroy2(),
    ?assertEqual([], W:all2()),

    %% resource server: {res, account}
    Res = {res, account},
    W:reg_server2(Res, ss_account, [#{res=>ss_account2}]),
    ?assertEqual(true, is_pid(W:find2(Res))),

    %% service server: {service, offline}
    W:reg_server2(?offline, ss_offline, [#{res=>ss_offline}]),
    ?assertEqual(true, is_pid(W:find2(?offline))),
    ?assertEqual(ok, W:call2(?offline, drop)),
    % erlang:display("world info >>>>>>>>>"),
    % erlang:display(ss_world:all2(?world)),
    % erlang:display("------------------------"),

    %% clear data
    ?assertEqual(ok, W:call2(Res, drop)),
    ?assertEqual([], W:call2(Res, all)),

    %% create an account with yifan
    M1 = W:call2(Res, [model, password]),
    D1 = #{account => "yifan", password => "123456"},
    {ok, Id1} = W:call2(Res, [create, D1, M1]),

    %% account must be uniq
    ?assertMatch({error, _}, W:call2(Res, [create, D1, M1])),

    %% 出席通知
    %%
    clear_msg(),

    %% 首先启动用户"adi"
    Adi = {account, "adi"},
    M2 = W:call2(Res, [model, all]),
    D2 = #{account => "adi", password => "123456", contacts => [{"yifan", #{rel=>double}}]},
    {ok, Id2} = W:call2(Res, [create, D2, M2]),
    W:call2(Res, [find, Id2]),
    W:reg_server2(Adi, ss_account, [#{res=>ss_account2}]),
    W:call2(Adi, [connect, self()]),
    ?assertMatch(not_login, W:call2(Adi, status)),
    ?assertMatch(ok, W:call2(Adi, [login, "adi", "123456"])),
    ?assertMatch("online", W:call2(Adi, status)),

    %% 添加订阅列表到"yifan"
    Yifan = {account, "yifan"},
    M3 = W:call2(Res, [model, contacts]),
    D3 = #{contacts => [{"adi", #{rel=>double}},
                        {"homeway", #{rel=>single}}]},
    ?assertMatch(ok, W:call2(Res, [update, Id1, D3, M3])),

    %% 出席
    W:reg_server2(Yifan, ss_account, [#{res=>ss_account2}]),
    W:call2(Yifan, [connect, self()]),
    ?assertMatch(not_login, W:call2(Yifan, contacts)),
    ?assertMatch(ok, W:call2(Yifan, [login, "yifan", "123456"])),

    %% 收取双方出席通知
    ?assertMatch({online, "yifan"}, got_msg()),
    ?assertMatch({online, "adi"}, got_msg()),

    %% 检查双方联系人出席情况
    Contacts1 = [{"yifan", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts1, W:call2(Adi, contacts)),
    Contacts2 = [{"adi", #{rel=>double, status=>"online"}},
                 {"homeway", #{rel=>single, status=>"offline"}}],
    ?assertEqual(Contacts2, W:call2(Yifan, contacts)),

    %% 添加联系人
    %%
    Homeway = {account, "homeway"},
    D4 = #{account => "homeway", password => "123456"},
    ?assertMatch({ok, _}, W:call2(Res, [create, D4, M1])),
    W:reg_server2(Homeway, ss_account, [#{res=>ss_account2}]),
    ?assertMatch(ok, W:call2(Homeway, [login, "homeway", "123456"])),

    %% homeway添加adi为联系人，双方在线
    % erlang:display("homeway add adi >> ........."),
    clear_msg(),
    ?assertEqual(ok, W:call2(Homeway, [invite, "adi"])),
    {invite_from, TrackId1, From1} = got_msg(),
    ?assertEqual(ok, W:call2(Adi, [invite_to_accept, TrackId1, From1])),
    Contacts4 = [{"adi", #{rel=>double, status=>"online"}}],
    receive nothing -> wait after 100 -> ok end,
    ?assertEqual(Contacts4, W:call2(Homeway, contacts)),
    Contacts5 = [{"homeway", #{rel=>double, status=>"online"}},
                 {"yifan", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts5, W:call2(Adi, contacts)),

    %% adi添加wenxuan为联系人，但wenxuan不在线
    Wenxuan = {account, "wenxuan"},
    D6 = #{account => "wenxuan", password => "123456"},
    ?assertMatch({ok, _}, W:call2(Res, [create, D6, M1])),
    ?assertEqual(ok, W:call2(Adi, [invite, "wenxuan"])),
    clear_msg(),
    %% wenxuan登录前, wenxuan为离线状态，关系为single
    Contacts6 = [{"wenxuan", #{rel=>single, status=>"offline"}},
                 {"homeway", #{rel=>double, status=>"online"}},
                 {"yifan", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts6, W:call2(Adi, contacts)),
    %% wenxuan登录后
    W:reg_server2(Wenxuan, ss_account, [#{res=>ss_account2}]),
    W:call2(Wenxuan, [connect, self()]),
    clear_msg(),
    ?assertMatch(ok, W:call2(Wenxuan, [login, "wenxuan", "123456"])),
    % erlang:display("wenxuan login ....."),
    %% wenxuan收到离线邀请, 接受
    {invite_from, TrackId6, From6} = got_msg(),
    ?assertEqual(ok, W:call2(Wenxuan, [invite_to_accept, TrackId6, From6])),
    receive nothing -> wait after 100 -> ok end,
    Contacts7 = [{"adi", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts7, W:call2(Wenxuan, contacts)),
    %% adi也更新联系人信息
    Contacts8 = [{"wenxuan", #{rel=>double, status=>"online"}},
                 {"homeway", #{rel=>double, status=>"online"}},
                 {"yifan",   #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts8, W:call2(Adi, contacts)),
    %% 离线通知应该已经清理
    ?assertEqual([], W:call2(?offline, all)),

    %% 拒绝联系人邀请
    %% adi添加xiaojie为联系人
    Xiaojie = {account, "xiaojie"},
    D8 = #{account => "xiaojie", password => "123456"},
    ?assertMatch({ok, _}, W:call2(Res, [create, D8, M1])),
    W:reg_server2(Xiaojie, ss_account, [#{res=>ss_account2}]),
    W:call2(Xiaojie, [connect, self()]),
    ?assertMatch(ok, W:call2(Xiaojie, [login, "xiaojie", "123456"])),
    clear_msg(),
    ?assertEqual(ok, W:call2(Adi, [invite, "xiaojie"])),
    %% xiaojie收到邀请, 拒绝
    {invite_from, TrackId8, From8} = got_msg(),
    ?assertEqual(ok, W:call2(Xiaojie, [invite_to_refuse, TrackId8, From8])),
    receive nothing -> wait after 100 -> ok end,
    ?assertEqual([], W:call2(Xiaojie, contacts)),

    %% 考虑发起邀请的人离线后再登录
    Zhuhao = {account, "zhuhao"},
    D9 = #{account => "zhuhao", password => "123456"},
    ?assertMatch({ok, _}, W:call2(Res, [create, D9, M1])),
    W:reg_server2(Zhuhao, ss_account, [#{res=>ss_account2}]),
    W:call2(Zhuhao, [connect, self()]),
    %% xiaoije邀请zhuhao为好友, 然后离线
    ?assertEqual(ok, W:call2(Xiaojie, [invite, "zhuhao"])),
    ?assertEqual(ok, W:unreg2(Xiaojie)),
    %% zhuhao登录, 收到邀请, 同意, 但此时xiaojie已离线
    clear_msg(),
    ?assertMatch(ok, W:call2(Zhuhao, [login, "zhuhao", "123456"])),
    {invite_from, TrackId9, From9} = got_msg(),
    ?assertEqual(ok, W:call2(Zhuhao, [invite_to_accept, TrackId9, From9])),
    %% xiaojie重新登录, 应仍能正确获得联系人状态
    W:reg_server2(Xiaojie, ss_account, [#{res=>ss_account2}]),
    ?assertMatch(ok, W:call2(Xiaojie, [login, "xiaojie", "123456"])),
    receive nothing -> wait after 100 -> ok end,
    Contacts9 = [{"zhuhao", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts9, W:call2(Xiaojie, contacts)),

    %% stop the world
    ?assertEqual(ok, W:call2(Res, drop)),
    ?assertEqual(ok, W:call2(?offline, drop)),
    W:stop2().

%% 如果没有任何消息就停顿100毫秒再确定消息已清除
clear_msg() ->
    receive _ -> clear_msg()
    after 100 -> ok
    end.

got_msg() ->
    receive Msg -> Msg
    after 200 -> nothing
    end.