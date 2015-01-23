%% -*- mode: nitrogen -*-
-module(ss_utils).

%% API
-export([print/1]).
-export([confirm_sync/2]).

%% print model pretty
% #{
%   nosqlite => [table, user],
%   index => [
%     {"type", #{to_display => true,to_query => true,type => text,value => <<"common">>}},
%     {"account", #{id => account,to_display => true,to_query => true,type => text}},
%     {"tel", #{to_display => true,to_query => true,type => text}},
%     {name, #{to_display => true,to_query => true,type => text}},
%     {"nickname", #{id => account,to_display => true,to_query => true,type => text}},
%     {"备注", #{to_display => true,to_query => true,type => text}}
%   ]}
print(Term) when is_map(Term) -> io:format(print_map(Term));
print(Term) when is_list(Term) -> io:format(lists:flatten("[\n\r" ++ print_item(Term, "  ") ++ "]\n\r")).

%% face和handlers
print_map(M) when is_map(M) ->
    S1 = "#{\n\r",
    L = maps:to_list(M),
    Items = lists:map(fun({K, V}) ->
        lists:flatten(io_lib:format("  ~ts => ", [print_key(K)]) ++ print_item(V, " "))
    end, L),
    S2 = string:join(Items, ",\n\r"),
    S1 ++ lists:flatten(S2) ++ "}\n\r".

%% 属性列表
print_item(L, LevelString) when is_list(L) ->
    Items = lists:map(fun({K, V}) ->
        LevelString ++ io_lib:format("{~ts, ~p}", [print_key(K), V])
    end, L),
    string:join(Items, ",\n\r") ++ "\n\r";
print_item(Term, LevelString) ->
    LevelString ++ io_lib:format("~p", [Term]).

%% Key名称转换
print_key(S) when is_list(S) ->
    Bs = unicode:characters_to_binary(S),
    <<"\"", Bs/binary, "\"">>;
print_key(B) when is_binary(B) ->
    <<"<<\"", B/binary, "\">>">>;
print_key(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
print_key(Term) -> Term.


%% 用来同步索引的更新
%% 每200毫秒查询一下是否同步
%% 最多查询5次
confirm_sync(From, Fun) ->
    spawn(fun() -> timer(From, Fun, 200, 10) end).

timer(From, Fun, Timeout, Maxtimes) ->
    receive
        canel ->
            void
    after
        Timeout ->
            case Maxtimes =< 0 of
                true ->
                    From ! timeout;
                _ ->
                    case Fun() of
                        true -> From ! ok;
                        false -> timer(From, Fun, Timeout, Maxtimes - 1)
                    end
            end
    end.