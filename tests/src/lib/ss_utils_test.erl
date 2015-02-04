%% -*- mode: nitrogen -*-
-module(ss_utils_test).
-include_lib("eunit/include/eunit.hrl").

print_test() ->
    L = [
        {
            "[a, b]",
            [a, b]
        },
        {
            "[\"abc\", b]",
            ["abc", b]
        },
        {
            "[<<\"a\">>, b]",
            [<<"a">>, b]
        },
        {
            "#{age => 9, name => yifan}",
            #{age=>9, name=>yifan}
        },
        {
            "#{\"name\" => \"一凡\", <<\"age\">> => 9}",
            #{<<"age">> =>9, "name"=>"一凡"}
        },
        {
            "[{age, 9}, {<<\"name\">>, <<\"一凡\">>}]",
            [{age, 9}, {<<"name">>, <<"一凡"/utf8>>}]
        },
        {
            "[hello, #{firends => [zhuwenxuan, yanxiaojie], \"name\" => yifan, <<\"age\">> => 9}]",
            [hello, #{<<"age">> =>9, "name"=>yifan, firends=>[zhuwenxuan, yanxiaojie]}]
        }
    ],
    [?assertEqual(E, ss_utils:to_string(V)) || {E, V} <- L].

