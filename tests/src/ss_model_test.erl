%% -*- mode: nitrogen -*-
-module(ss_model_test).
-export([model/0]).
-include_lib("eunit/include/eunit.hrl").

%% 定义示例模型(用户信息)
model() -> [
    {"类型", #{value=>"普通会员"}},
    {"账户名", #{value=>"yifan"}},
    {"性别", #{value=>"女"}},
    {"密码", #{type=>password, value=>"123456"}},
    {"EMail", #{}},
    {"电话", #{}},
    {"姓名", #{}},
    {"昵称", #{}},
    {"头像", #{type=>link, value=>"/themes/cube/img/samples/scarlet-159.png"}},
    {"生日", #{type=>date}},
    {"联系人", #{type=>tags}}
].

%% 运行关联测试
to_test() ->
    ok.

drop_test() ->
    M1 = ss_model:drop(model(), ["类型", "电话", "密码", "联系人", "生日"]),
    M2 = [
        {"账户名", #{value=>"yifan"}},
        {"性别", #{value=>"女"}},
        {"EMail", #{}},
        {"姓名", #{}},
        {"昵称", #{}},
        {"头像", #{type=>link, value=>"/themes/cube/img/samples/scarlet-159.png"}}
    ],
    ?assertEqual(M1, M2).