%% -*- mode: nitrogen -*-
-module(ss_model_test).
-export([model/0, custom_validate/2]).
-include_lib("eunit/include/eunit.hrl").

%% 定义示例模型(用户信息)
custom_validate(K, M) ->
    ss_model:custom_validate(K, <<"长度必须为4"/utf8>>, fun() ->
        ss_model:length(K, M) == 4
    end).

model() -> ss_model:confirm_model([
    {type, #{value=>"普通会员"}},
    {account, #{value=>"yifan"}},
    {sex, #{value=>"女"}},
    {password, #{type=>password, value=>"123456"}},
    {email, #{validate=>[required]}},
    {tel, #{}},
    {name, #{}},
    {nickname, #{}},
    {avata, #{type=>link, value=>"/themes/cube/img/samples/scarlet-159.png"}},
    {birthday, #{validate=> [fun ?MODULE:custom_validate/2]}},
    {contacts, #{type=>tags}}
]).

filter_test() ->
    M1 = ss_model:filter([account, email, avata], model()),
    M2 = ss_model:confirm_model([
        {account, #{value=>"yifan"}},
        {email, #{validate=>[required]}},
        {avata, #{type=>link, value=>"/themes/cube/img/samples/scarlet-159.png"}}
    ]),
    ?assertEqual(M1, M2).

drop_test() ->
    M1 = ss_model:drop([type, tel, password, contacts, birthday], model()),
    M2 = ss_model:confirm_model([
        {account, #{value=>"yifan"}},
        {sex, #{value=>"女"}},
        {email, #{validate=>[required]}},
        {name, #{}},
        {nickname, #{}},
        {avata, #{type=>link, value=>"/themes/cube/img/samples/scarlet-159.png"}}
    ]),
    ?assertEqual(M1, M2).

set_test() ->
    M1 = ss_model:filter([account, password], model()),
    M2 = ss_model:set([{account, "adi"}, {password, "aabbcc"}], M1),
    M3 = ss_model:confirm_model([
        {account, #{value=>"adi"}},
        {password, #{type=>password, value=>"aabbcc"}}
    ]),
    ?assertEqual(M2, ss_model:confirm_model(M3)).
