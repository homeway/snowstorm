%% -*- mode: nitrogen -*-
-module(ss_account_test).
-include_lib("eunit/include/eunit.hrl").
-define(offline, {service, offline}).

prepare() ->
    %% 启动world
    W = ss:world(to_test_ss_account),
    W:start(),
    W:destroy(),
    ?assertEqual([], W:all()),

    %% resource server: {res, account}
    Db = ss:nosqlite(ss_account_test_data),
    Res = {res, account},
    Accounts = W:reg_server(Res, ss_account, [#{db=>Db}]),

    {W, Accounts, Res, Db}.

finish(W, Res) ->
    %% stop the world
    ?assertEqual(ok, W:call(Res, drop)),
    ?assertEqual(ok, W:call(?offline, drop)),
    W:stop().

account_test() ->
    {W, Accounts, Res, Db} = prepare(),
    ?assertEqual(true, is_pid(W:find(Res))),

    %% service server: {service, offline}
    W:reg_server(?offline, ss_offline, [#{db=>ss:nosqlite(ss_account_test_offline)}]),
    ?assertEqual(true, is_pid(W:find(?offline))),
    ?assertEqual(ok, W:call(?offline, drop)),
    % erlang:display("world info >>>>>>>>>"),
    % erlang:display(ss_world:all2(?world)),
    % erlang:display("------------------------"),

    %% clear data
    ?assertEqual(ok, Accounts:call(drop)),
    ?assertEqual([], Accounts:call(all)),

    %% create an account with yifan
    M1 = Accounts:call([model, password]),
    D1 = #{account => "yifan", password => "123456"},
    {ok, Id1} = Accounts:call([create, D1, M1]),

    %% account must be uniq
    ?assertMatch({error, _}, Accounts:call([create, D1, M1])),

    %% 出席通知
    %%
    clear_msg(),

    %% 首先启动用户"adi"
    Adi = {account, "adi"},
    M2 = W:call(Res, [model, all]),
    D2 = #{account => "adi", password => "123456", contacts => [{"yifan", #{rel=>double}}]},
    {ok, Id2} = W:call(Res, [create, D2, M2]),
    W:call(Res, [find, Id2]),
    W:reg_server(Adi, ss_account, [#{db=>Db}]),
    W:call(Adi, [connect, self()]),
    ?assertMatch(not_login, W:call(Adi, status)),
    ?assertMatch(ok, W:call(Adi, [login, "adi", "123456"])),
    ?assertMatch("online", W:call(Adi, status)),

    %% 添加订阅列表到"yifan"
    Yifan = {account, "yifan"},
    M3 = W:call(Res, [model, contacts]),
    D3 = #{contacts => [{"adi", #{rel=>double}},
                        {"homeway", #{rel=>single}}]},
    ?assertMatch(ok, W:call(Res, [update, Id1, D3, M3])),

    %% 出席
    W:reg_server(Yifan, ss_account, [#{db=>Db}]),
    W:call(Yifan, [connect, self()]),
    ?assertMatch(not_login, W:call(Yifan, contacts)),
    ?assertMatch(ok, W:call(Yifan, [login, "yifan", "123456"])),

    %% 收取双方出席通知
    ?assertMatch({online, "yifan"}, got_msg()),
    ?assertMatch({online, "adi"}, got_msg()),

    %% 检查双方联系人出席情况
    Contacts1 = [{"yifan", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts1, W:call(Adi, contacts)),
    Contacts2 = [{"adi", #{rel=>double, status=>"online"}},
                 {"homeway", #{rel=>single, status=>"offline"}}],
    ?assertEqual(Contacts2, W:call(Yifan, contacts)),

    %% 添加联系人
    %%
    Homeway = {account, "homeway"},
    D4 = #{account => "homeway", password => "123456"},
    ?assertMatch({ok, _}, W:call(Res, [create, D4, M1])),
    W:reg_server(Homeway, ss_account, [#{db=>Db}]),
    ?assertMatch(ok, W:call(Homeway, [login, "homeway", "123456"])),

    %% homeway添加adi为联系人，双方在线
    % erlang:display("homeway add adi >> ........."),
    clear_msg(),
    ?assertEqual(ok, W:call(Homeway, [invite, "adi"])),
    {invite_from, TrackId1, From1} = got_msg(),
    ?assertEqual(ok, W:call(Adi, [invite_to_accept, TrackId1, From1])),
    Contacts4 = [{"adi", #{rel=>double, status=>"online"}}],
    receive nothing -> wait after 100 -> ok end,
    ?assertEqual(Contacts4, W:call(Homeway, contacts)),
    Contacts5 = [{"homeway", #{rel=>double, status=>"online"}},
                 {"yifan", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts5, W:call(Adi, contacts)),

    %% adi添加wenxuan为联系人，但wenxuan不在线
    Wenxuan = {account, "wenxuan"},
    D6 = #{account => "wenxuan", password => "123456"},
    ?assertMatch({ok, _}, W:call(Res, [create, D6, M1])),
    ?assertEqual(ok, W:call(Adi, [invite, "wenxuan"])),
    clear_msg(),
    %% wenxuan登录前, wenxuan为离线状态，关系为single
    Contacts6 = [{"wenxuan", #{rel=>single, status=>"offline"}},
                 {"homeway", #{rel=>double, status=>"online"}},
                 {"yifan", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts6, W:call(Adi, contacts)),
    %% wenxuan登录后
    W:reg_server(Wenxuan, ss_account, [#{db=>Db}]),
    W:call(Wenxuan, [connect, self()]),
    clear_msg(),
    ?assertMatch(ok, W:call(Wenxuan, [login, "wenxuan", "123456"])),
    % erlang:display("wenxuan login ....."),
    %% wenxuan收到离线邀请, 接受
    {invite_from, TrackId6, From6} = got_msg(),
    ?assertEqual(ok, W:call(Wenxuan, [invite_to_accept, TrackId6, From6])),
    receive nothing -> wait after 100 -> ok end,
    Contacts7 = [{"adi", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts7, W:call(Wenxuan, contacts)),
    %% adi也更新联系人信息
    Contacts8 = [{"wenxuan", #{rel=>double, status=>"online"}},
                 {"homeway", #{rel=>double, status=>"online"}},
                 {"yifan",   #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts8, W:call(Adi, contacts)),
    %% 离线通知应该已经清理
    ?assertEqual([], W:call(?offline, all)),

    %% 拒绝联系人邀请
    %% adi添加xiaojie为联系人
    Xiaojie = {account, "xiaojie"},
    D8 = #{account => "xiaojie", password => "123456"},
    ?assertMatch({ok, _}, W:call(Res, [create, D8, M1])),
    W:reg_server(Xiaojie, ss_account, [#{db=>Db}]),
    W:call(Xiaojie, [connect, self()]),
    ?assertMatch(ok, W:call(Xiaojie, [login, "xiaojie", "123456"])),
    clear_msg(),
    ?assertEqual(ok, W:call(Adi, [invite, "xiaojie"])),
    %% xiaojie收到邀请, 拒绝
    {invite_from, TrackId8, From8} = got_msg(),
    ?assertEqual(ok, W:call(Xiaojie, [invite_to_refuse, TrackId8, From8])),
    receive nothing -> wait after 100 -> ok end,
    ?assertEqual([], W:call(Xiaojie, contacts)),

    %% 考虑发起邀请的人离线后再登录
    Zhuhao = {account, "zhuhao"},
    D9 = #{account => "zhuhao", password => "123456"},
    ?assertMatch({ok, _}, W:call(Res, [create, D9, M1])),
    Zhu = W:reg_server(Zhuhao, ss_account, [#{db=>Db}]),
    Zhu:call([connect, self(), fun(Msg) -> {server, Msg} end]),
    %% xiaoije邀请zhuhao为好友, 然后离线
    ?assertEqual(ok, W:call(Xiaojie, [invite, "zhuhao"])),
    ?assertEqual(ok, W:unreg(Xiaojie)),
    %% zhuhao登录, 收到邀请, 同意, 但此时xiaojie已离线
    clear_msg(),
    ?assertMatch(ok, Zhu:call([login, "zhuhao", "123456"])),
    {server, {invite_from, TrackId9, From9}} = got_msg(),
    ?assertEqual(ok, Zhu:call([invite_to_accept, TrackId9, From9])),
    %% xiaojie重新登录, 应仍能正确获得联系人状态
    W:reg_server(Xiaojie, ss_account, [#{db=>Db}]),
    ?assertMatch(ok, W:call(Xiaojie, [login, "xiaojie", "123456"])),
    receive nothing -> wait after 100 -> ok end,
    Contacts9 = [{"zhuhao", #{rel=>double, status=>"online"}}],
    ?assertEqual(Contacts9, W:call(Xiaojie, contacts)),

    %% zhuhao发给xiaojie在线消息
    clear_msg(),
    W:call(Xiaojie, [connect, self()]),
    Zhu:send([chat, "hello", "xiaojie"]),
    ?assertEqual({chat, "hello", "zhuhao"}, got_msg()),

    %% xiaojie下线，zhuhao收到消息
    W:call(Xiaojie, logout),
    ?assertEqual({server, {offline, "xiaojie"}}, got_msg()),

    finish(W, Res).

%% 如果没有任何消息就停顿100毫秒再确定消息已清除
clear_msg() ->
    receive _ -> clear_msg()
    after 50 -> ok
    end.

got_msg() ->
    receive Msg -> Msg
    after 50 -> nothing
    end.