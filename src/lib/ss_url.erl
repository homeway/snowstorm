%% -*- mode: nitrogen -*-
-module(ss_url).

%% API
-export([url/1, url/2, url/3, url/4, url2/2]).

%% @doc 构建简单的url
%% url(project, post)
%%   <<"/project/post">>
%% @end
            url(T1) -> url2([T1], []).
        url(T1, T2) -> url2([T1, T2], []).
    url(T1, T2, T3) -> url2([T1, T2, T3], []).
url(T1, T2, T3, T4) -> url2([T1, T2, T3, T4], []).

%% @doc 构建带参数的url
%% url2([project, post], [{tag, "car"}])
%%   <<"/project/post?tag=car">>

%% url2([my_project, post], [{tag, "car"}])
%%   <<"/my/project/post?tag=car">>

%% url2(["my_project", post], [{tag, "car"}])
%%   <<"/my_project/post?tag=car">>
%% @end
url2(L1, L2) ->
    NewList = lists:map(fun(I) ->
        if
            is_atom(I) ->
                Tokens = string:tokens(atom_to_list(I), "_"),
                lists:map(fun(T) -> ss:to_binary(T) end, Tokens);
            true -> ss:to_binary(I)
        end
    end, L1),
    (url_acc(lists:flatten(NewList), L2, [], [])).

url_acc([], [], Tokens1, Tokens2) ->
    S1 = string:join([""|lists:reverse(Tokens1)], "/"),
    S2 = case Tokens2 of
        [] -> S1;
        _ ->  S1 ++ "?" ++ string:join(lists:reverse(Tokens2), "&")
    end,
    ss:to_binary(S2);
url_acc([], [{K, V}|QueryParams], Tokens1, Tokens2) ->
    S = io_lib:format("~ts=~ts", [ss:to_binary(K), ss:to_binary(V)]),
    url_acc([], QueryParams, Tokens1, [S|Tokens2]);
url_acc([H|T], QueryParams, Tokens1, Tokens2) ->
    S = io_lib:format("~s", [H]),
    url_acc(T, QueryParams, [S|Tokens1], Tokens2).
