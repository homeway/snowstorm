%% -*- mode: nitrogen -*-
-module(ss_validate_test).
-export([custom_validate/2, model/0]).
-include_lib("eunit/include/eunit.hrl").

-define(db, ss_nosqlite).
-define(res, ss_validate_user).

%% 定义示例模型(用户信息)
custom_validate(K, M) ->
    ss_validate:custom(K, <<"长度必须为4"/utf8>>, fun() ->
        ss_model:length(K, M) == 4
    end).

model() -> ss_model:confirm_model([
    {account, #{validate=>[required, uniq]}},
    {sex, #{value=>"女"}},
    {email, #{validate=>[required]}},
    {birthday, #{validate=> [fun ?MODULE:custom_validate/2]}}
]).

check_model_test() ->
    ?db:drop(?res),
    M0 = ss_model:set([{account, "yifan"}], model()),
    {error, M1} = ss_validate:check(M0, #{db=>?db, res=>?res}),
    M2 = ss_model:confirm_model([
        {account, #{value=>"yifan", validate=>[required, uniq]}},
        {sex, #{value=>"女"}},
        {email, #{validate=>[required], error=> <<"字段不能为空"/utf8>>}},
        {birthday, #{
            validate=> [fun ?MODULE:custom_validate/2],
            error=> <<"长度必须为4"/utf8>>}}
    ]),
    ?assertEqual(M1, M2).
check_db_test() ->
    ?db:drop(?res),
    D0 = #{account=>"yifan", email=>"yifan@gmail", birthday=>"0701"},
    M0 = model(),
    S = #{db=>?db, res=>?res},
    ?assertMatch({ok, _}, ss_validate:check(ss_model:to_model(D0, M0), S)),
    {ok, _} = ?db:create(?res, D0),
    ?assertMatch({error, _}, ss_validate:check(ss_model:to_model(D0, M0), S)).
