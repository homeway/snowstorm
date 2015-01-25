%% -*- mode: nitrogen -*-
-module(model_user).
-export([m/1]).
-export([create/1, update/1, delete/1, get/1, all/0]).
-define(db, ss_nosqlite).
-define(res, user).

m() -> [
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

m(all)      -> ss_model:confirm_model(m());
m(new)      -> m(all);
m(index)    -> ss_model:filter(m(), ["账户名", "昵称"]);
m(edit)     -> ss_model:drop(m(), ["账户名", "类型"]);
m(show)     -> ss_model:drop(m(), ["密码"]);
m(password) -> ss_model:filter(m(), ["账户名", "密码"]);
m(profile)  -> ss_model:filter([m(), "账户名", "昵称", "头像"]).

%% db的非标操作也应在此定义
create(M)   -> ?db:create(?res, M).
update(M)   -> ?db:patch (?res, M).
delete(Key) -> ?db:delete(?res, Key).
get(Key)    -> ?db:get   (?res, Key, m(show)).
all()       -> ?db:all   (?res, m(all)).