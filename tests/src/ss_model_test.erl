%% -*- mode: nitrogen -*-
-module(ss_model_test).
-export([model/0, custom_validate/2]).
-include_lib("eunit/include/eunit.hrl").

%% 定义示例模型(用户信息)
custom_validate(K, M) ->
    case ss_model:length(K, M) == 4 of
        true -> [];
        false -> [{K, <<"长度必须为4"/utf8>>}]
    end.
    
model() -> ss_model:confirm_model([
    {"类型", #{value=>"普通会员"}},
    {"账户名", #{value=>"yifan"}},
    {"性别", #{value=>"女"}},
    {"密码", #{type=>password, value=>"123456"}},
    {"EMail", #{validate=>[required]}},
    {"电话", #{}},
    {"姓名", #{}},
    {"昵称", #{}},
    {"头像", #{type=>link, value=>"/themes/cube/img/samples/scarlet-159.png"}},
    {"生日", #{validate=> [fun ?MODULE:custom_validate/2]}},
    {"联系人", #{type=>tags}}
]).

%% 运行关联测试
% to_test() ->
%     ok.

filter_test() ->
    M1 = ss_model:filter(model(), ["账户名", "EMail", "头像"]),
    M2 = [
        {<<"账户名"/utf8>>, #{value=>"yifan"}},
        {<<"EMail"/utf8>>, #{validate=>[required]}},
        {<<"头像"/utf8>>, #{type=>link, value=>"/themes/cube/img/samples/scarlet-159.png"}}
    ],
    ?assertEqual(M1, M2).

drop_test() ->
    M1 = ss_model:drop(model(), ["类型", "电话", "密码", "联系人", "生日"]),
    M2 = [
        {<<"账户名"/utf8>>, #{value=>"yifan"}},
        {<<"性别"/utf8>>, #{value=>"女"}},
        {<<"EMail"/utf8>>, #{validate=>[required]}},
        {<<"姓名"/utf8>>, #{}},
        {<<"昵称"/utf8>>, #{}},
        {<<"头像"/utf8>>, #{type=>link, value=>"/themes/cube/img/samples/scarlet-159.png"}}
    ],
    ?assertEqual(M1, M2).

validate_test() ->
    {error, M1} = ss_model:validate(ss_model:filter(model(), ["生日"])),
    M2 = [
        {<<"生日"/utf8>>, #{
            validate=> [fun ?MODULE:custom_validate/2],
            error=> <<"长度必须为4"/utf8>>}}
    ],
    ?assertEqual(M1, M2).