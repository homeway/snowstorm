%% -*- mode: nitrogen -*-
-module(ss_utils).

%% API
-export([print/1]).
-export([confirm_sync/2]).

%% print model pretty
% #{
%   '_handlers' => [
%     {nosqlite, #{table => user}}
%   ],
%   index => [
%     {"type", #{to_display => true,to_query => true,type => text,value => <<"common">>}},
%     {"account", #{id => account,to_display => true,to_query => true,type => text}},
%     {"tel", #{to_display => true,to_query => true,type => text}},
%     {name, #{to_display => true,to_query => true,type => text}},
%     {"nickname", #{id => account,to_display => true,to_query => true,type => text}},
%     {"备注", #{to_display => true,to_query => true,type => text}}
%   ]}
print(Models) when is_map(Models) -> io:format(pa1(Models));
print(Model) when is_list(Model) -> io:format(lists:flatten(pa2(Model, ""))).

%% face和handlers
pa1(M) when is_map(M) ->
    S1 = "#{\n\r",
    L = maps:to_list(M),
    Items = lists:map(fun({K, V}) ->
        lists:flatten(io_lib:format("  ~p => [\n\r", [K]) ++ pa2(V, "    ") ++ "  ]")
    end, L),
    S2 = string:join(Items, ",\n\r"),
    S1 ++ lists:flatten(S2) ++ "}\n\r".

%% 属性列表
pa2(L, LevelString) when is_list(L) ->
    Items = lists:map(fun({K, V}) ->
        LevelString ++ io_lib:format("{~ts, ~p}", [pa3(K), V])
    end, L),
    string:join(Items, ",\n\r") ++ "\n\r";
pa2(Term, LevelString) ->
    LevelString ++ io_lib:format("~p", [Term]).

%% Key名称转换
pa3(S) when is_list(S) ->
    Bs = unicode:characters_to_binary(S),
    <<"\"", Bs/binary, "\"">>;
pa3(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
pa3(Term) -> Term.


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