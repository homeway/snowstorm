%% -*- mode: nitrogen -*-
-module(ss_server_test).
-include_lib("eunit/include/eunit.hrl").

to_test() ->
    %% 启动world
    World = ss:world(to_test_ss_server),
    World:start(),
    World:destroy(),
    ?assertEqual([], World:all()),

    %% reg a res server
    Users = World:reg_server(res_to_test, ss_user2),

    %% call info
    ?assertMatch(#{db:=_Db}, Users:call(info)),

    %% call model
    ?assertMatch([], Users:call(model, undefined)),
    ?assertMatch([{_K, #{}}|_], Users:call(model, [all])),


    %% call db methods

    %% clear data
    ?assertEqual(ok, Users:call(drop)),
    ?assertEqual([], Users:call(all)),

    %% create
    M1 = Users:call(model, [password]),
    D1 = #{<<"账户名"/utf8>> => "yifan", <<"密码"/utf8>> => "123456"},
    ?assertMatch([{<<"账户名"/utf8>>, _}, {<<"密码"/utf8>>, _}], M1),
    {ok, Id1} = Users:call(create, [D1, M1]),
    ?assertEqual(true, is_binary(Id1)),

    %% create with invalidte min length
    D2 = #{<<"账户名"/utf8>> => "yifan", <<"密码"/utf8>> => "1234"},
    {error, M2} = Users:call(create, [D2, M1]),
    ?assertMatch([{<<"账户名"/utf8>>, #{}}, {<<"密码"/utf8>>, #{}}], M2),
    {error, _} = Users:call(create, [D2, password]),
    ?assertMatch([{<<"账户名"/utf8>>, #{}}, {<<"密码"/utf8>>, #{}}], M2),

    %% find Id1
    ?assertMatch(#{<<"账户名"/utf8>> := "yifan", <<"密码"/utf8>> := "123456"}, Users:call(find, Id1)),

    %% update Id1
    M_PassOnly = ss_model:filter(["密码"], ss_user2:model(all)),
    D3 = #{<<"密码"/utf8>> => "654321"},
    ?assertEqual(ok, Users:call(update, [Id1, D3, M_PassOnly])),
    ?assertMatch(#{<<"账户名"/utf8>> := "yifan", <<"密码"/utf8>> := "654321"}, Users:call(find, Id1)),

    %% other methods
    %%
    %% hello
    ?assertEqual(not_login, Users:call(hello)),

    %% login
    ?assertMatch({error, "账户不存在"}, Users:call(login, ["nothisuser", "111111"])),
    ?assertMatch({error, "密码不正确"}, Users:call(login, ["yifan", "111111"])),
    ?assertEqual(ok, Users:call(login, ["yifan", "654321"])),

    %% status
    ?assertMatch("已登录", Users:call(status)),
    ?assertMatch(ok, Users:call(status, ["忙碌"])),
    ?assertMatch("忙碌", Users:call(status)),

    %% who
    ?assertMatch(Id1, Users:call(who)),
    ?assertMatch(ok, Users:call(logout)),
    ?assertMatch(not_login, Users:call(who)),

    %% all
    ?assertEqual(1, length(Users:call(all))),
    {ok, _Id2} = Users:call(create, [D1, M1]),
    {ok, Id3} = Users:call(create, [D1, M1]),
    ?assertEqual(3, length(Users:call(all))),

    %% delete Id3
    ?assertEqual(ok, Users:call(delete, [Id3])),
    ?assertEqual(2, length(Users:call(all))),
    ?assertMatch(notfound, Users:call(find, Id3)),

    %% stop the world
    ?assertEqual(ok, Users:call(drop)),
    World:stop().
