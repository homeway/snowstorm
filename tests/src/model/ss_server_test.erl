%% -*- mode: nitrogen -*-
-module(ss_server_test).
-include_lib("eunit/include/eunit.hrl").

to_test() ->
    %% 启动world
    W = ss:world(to_test_ss_server),
    W:start(),
    W:destroy(),
    ?assertEqual([], W:all()),

    %% regss ss_user1
    P1 = test_user1,
    W:reg_server(P1, ss_user2),
    ?assertEqual(true, is_pid(W:find(P1))),

    %% call info
    ?assertMatch(#{db:=_Db, res:=_Res}, W:call(P1, info)),

    %% call model
    ?assertMatch([], W:call(P1, [model, undefined])),
    ?assertMatch([{_K, #{}}|_], W:call(P1, [model, all])),

    %% call db methods

    %% clear data
    ?assertEqual(ok, W:call(P1, drop)),
    ?assertEqual([], W:call(P1, all)),

    %% create
    M1 = W:call(P1, [model, password]),
    D1 = #{<<"账户名"/utf8>> => "yifan", <<"密码"/utf8>> => "123456"},
    ?assertMatch([{<<"账户名"/utf8>>, _}, {<<"密码"/utf8>>, _}], M1),
    {ok, Id1} = W:call(P1, [create, D1, M1]),
    ?assertEqual(true, is_binary(Id1)),

    %% create with invalidte min length
    D2 = #{<<"账户名"/utf8>> => "yifan", <<"密码"/utf8>> => "1234"},
    {error, M2} = W:call(P1, [create, D2, M1]),
    ?assertMatch([{<<"账户名"/utf8>>, #{}}, {<<"密码"/utf8>>, #{}}], M2),
    {error, _} = W:call(P1, [create, D2, password]),
    ?assertMatch([{<<"账户名"/utf8>>, #{}}, {<<"密码"/utf8>>, #{}}], M2),

    %% find Id1
    ?assertMatch(#{<<"账户名"/utf8>> := "yifan", <<"密码"/utf8>> := "123456"}, W:call(P1, [find, Id1])),

    %% update Id1
    M_PassOnly = ss_model:filter(["密码"], ss_user2:model(all)),
    D3 = #{<<"密码"/utf8>> => "654321"},
    ?assertEqual(ok, W:call(P1, [update, Id1, D3, M_PassOnly])),
    ?assertMatch(#{<<"账户名"/utf8>> := "yifan", <<"密码"/utf8>> := "654321"}, W:call(P1, [find, Id1])),

    %% other methods
    %%
    %% hello
    ?assertEqual(not_login, W:call(P1, hello)),

    %% login
    ?assertMatch({error, "账户不存在"}, W:call(P1, [login, "nothisuser", "111111"])),
    ?assertMatch({error, "密码不正确"}, W:call(P1, [login, "yifan", "111111"])),
    ?assertEqual(ok, W:call(P1, [login, "yifan", "654321"])),

    %% status
    ?assertMatch("已登录", W:call(P1, status)),
    ?assertMatch(ok, W:call(P1, [status, "忙碌"])),
    ?assertMatch("忙碌", W:call(P1, status)),

    %% who
    ?assertMatch(Id1, W:call(P1, who)),
    ?assertMatch(ok, W:call(P1, logout)),
    ?assertMatch(not_login, W:call(P1, who)),

    %% all
    ?assertEqual(1, length(W:call(P1, all))),
    {ok, _Id2} = W:call(P1, [create, D1, M1]),
    {ok, Id3} = W:call(P1, [create, D1, M1]),
    ?assertEqual(3, length(W:call(P1, all))),

    %% delete Id3
    ?assertEqual(ok, W:call(P1, [delete, Id3])),
    ?assertEqual(2, length(W:call(P1, all))),
    ?assertMatch(notfound, W:call(P1, [find, Id3])),

    %% stop the world
    ?assertEqual(ok, W:call(P1, drop)),
    W:stop().
