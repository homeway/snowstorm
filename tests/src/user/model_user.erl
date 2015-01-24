%% -*- mode: nitrogen -*-
-module(model_user).
-export([model/1, action/2]).
-define(db, ss_nosqlite).
-define(res, user).

model() -> [
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
].
model(all) -> ss_model:confirm_model(model());
model(index) -> ss_model:filter(model(), ["账户名", "昵称"]);
model(new) -> ss_model:confirm_model(model());
model(edit) -> ss_model:drop(model(), ["账户名", "昵称"]);
model(show) -> ss_model:drop(model(), ["账户名"]).

action(create, M)   -> ?db:create(?res, M);
action(update, M)   -> ?db:patch (?res, M);
action(delete, Key) -> ?db:delete(?res, Key);
action(get, Key)    -> ?db:get   (?res, Key, model(all));
action(all, _)      -> ?db:all   (?res, model(all)).