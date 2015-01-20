%% -*- mode: nitrogen -*-
-module(ss_model_test).
-export([user/0]).
-include_lib("eunit/include/eunit.hrl").

%% 构造一个基于nosqlite的配置项模板
base(Table, Model) ->
    maps:merge(#{ '_handlers' => [{nosqlite, #{table=>Table} }] }, Model).

%% 定义示例模型(用户信息)
user() ->
    base(user,
        #{index => [
            {"type", text(#{value=> <<"common">>})},
            {"account" , text(#{id=>account})},
            {"tel", text()},
            {name, text()},
            {"nickname", text(#{id=>account})},
            {"备注", text()}
        ]}
    ).

%% 运行关联测试
to_test() ->
    ok.

%% 读取模型
get_test() ->
    User = ss_model:face(index, user()),
    ?assertMatch([{"type", _}|_], User).

%% 1. 普通取值
%% 2. 支持默认值
value_test() ->
    User = ss_model:face(index, user()),
    ?assertEqual(undefined, rico_model:value("account", User, undefined)),
    ?assertEqual(<<"common">>, rico_model:value("type", User)).

%% 直接将模型中的值与变量比较
equal_test() ->
    User = ss_model:face(index, user()),
    ?assertEqual(true, ss_model:equal("type", User, <<"common">>)).

%% test helper -------------------------------------------------------
text() ->
    text(#{}).
text(Field) ->
    Default = #{
        type=> text,
        to_display => true,
        to_query   => true
    },
    maps:merge(Default, Field).
