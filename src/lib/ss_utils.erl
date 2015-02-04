%% -*- mode: nitrogen -*-
-module(ss_utils).

%% API
-export([to_string/1]).
-export([confirm_sync/2]).

%% to replace erlang:display/1
to_string(L) -> lists:flatten(to_string(L, "")).

to_string(L, Acc0) when is_list(L) ->
    IsString = lists:all(fun(I) -> is_integer(I) end, L),
    case IsString of
        true ->
            Acc0 ++ "\"" ++ L ++ "\"";
        false ->
            L2 = lists:map(fun(I) ->
                to_string(I)
            end, L),
            Acc0 ++ "[" ++ string:join(L2, ", ") ++ "]"
    end;

to_string(M, Acc0) when is_map(M) ->
    L2 = lists:map(fun({K, V}) ->
        to_string(K) ++ " => " ++ to_string(V)
    end, maps:to_list(M)),
    Acc0 ++ "#{" ++ string:join(L2, ", ") ++ "}";

to_string(T, Acc0) when is_tuple(T) ->
    L2 = [to_string(I) || I <- tuple_to_list(T)],
    Acc0 ++ "{" ++ string:join(L2, ", ") ++ "}";

to_string(B, Acc0) when is_binary(B) ->
    Acc0 ++ io_lib:format("<<\"~ts\">>", [B]);

to_string(I, Acc0) when is_integer(I) ->
    Acc0 ++ io_lib:format("~B", [I]);

to_string(Term, Acc0) ->
    Acc0 ++ io_lib:format("~p", [Term]).

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