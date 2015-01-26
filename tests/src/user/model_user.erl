%% -*- mode: nitrogen -*-
-module(model_user).
-compile(export_all).

all() -> ss_model:confirm_model([
    {"类型", #{value=>"普通会员", validate=>[required]}},
    {"账户名", #{value=>"yifan", validate=>[required]}},
    {"性别", #{value=>"女"}},
    {"密码", #{type=>password, value=>"123456", validate=>[required, {min, 6}]}},
    {"EMail", #{}},
    {"电话", #{}},
    {"姓名", #{}},
    {"昵称", #{}},
    {"头像", #{type=>link, value=>"/themes/cube/img/samples/scarlet-159.png"}},
    {"生日", #{type=>date}},
    {"联系人", #{type=>tags}}
]).

new()      -> all().
index()    -> ss_model:filter(["账户名", "昵称"], all()).
edit()     -> ss_model:drop(["账户名", "类型"], all()).
show()     -> ss_model:drop(["密码"], all()).
password() -> ss_model:filter(["账户名", "密码"], all()).
profile()  -> ss_model:filter(["账户名", "昵称", "头像"], all()).

