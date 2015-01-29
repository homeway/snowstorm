%% -*- mode: nitrogen -*-
-module(ss_model).
-export([value/1, value/2, value/3, length/1, length/2, equal/2, equal/3]).
-export([confirm_model/1, confirm_list/1, filter/2, drop/2]).
-export([from_model/1, to_model/2, set/2]).

%% get value from field, model or map with key
%% model: [{K, map()}]
%%
%% from field
value(Field) when is_map(Field) -> maps:get(value, Field).
value(Field, Default) when is_map(Field) -> maps:get(value, Field, Default);
%% from maps
value(Key, Map) when is_map(Map) -> maps:get(ss:to_binary(Key), Map);
value(Key, Model) when is_list(Model) ->
    V = proplists:get_value(ss:to_binary(Key), Model),
    maps:get(value, V).
%% from model
value(Key, Map, Default) when is_map(Map) ->
    maps:get(ss:to_binary(Key), Map, Default);
value(Key, Model, Default) ->
    case proplists:get_value(ss:to_binary(Key), Model) of
        undefined -> Default;
        V -> maps:get(value, V, Default)
    end.

% length of value in field
length(Field) ->
    case maps:get(value, Field, <<>>) of
        <<>> -> 0;
        V -> erlang:length(ss:to_list(V))
    end.
%% length of value in model
length(Key, Model) ->
    case value(Key, Model, <<>>) of
        <<>> -> 0;
        V -> erlang:length(ss:to_list(V))
    end.

%% equal to value in field
equal(Field, ToCompare) ->
    value(Field, undefined) =:= ToCompare.
%% equal to value in model
equal(Key, Data, ToCompare) ->
    value(Key, Data, undefined) =:= ToCompare.

%% set value to existing model
%% 赋值队列必须为属性列表
set([], Model) -> Model;
set([{K1, V}|Rest], Model0) ->
    K = ss:to_binary(K1),
    VOld = proplists:get_value(K, Model0),
    VNew = VOld#{value => V},
    Model = lists:keyreplace(K, 1, Model0, {K, VNew}),
    set(Rest, Model).

%% convert model key to binary
confirm_model(M) -> [{ss:to_binary(K), V} || {K, V} <- M].
confirm_list(L) -> [ss:to_binary(K) || K <- L].

%% list filter and drop
filter(L1, M) ->
    L = confirm_list(L1),
    [{K, V} || {K, V} <- M, lists:any(fun(I) -> I =:= K end, L)].
drop(L1, M) ->
    L = confirm_list(L1),
    [{K, V} || {K, V} <- M, not(lists:any(fun(I) -> I =:= K end, L))].

%% convert model from db maps -------------------------------------
%% db maps: #{key() => value()}
from_model(M) ->
    L1 = lists:map(fun({K, V}) ->
        {ss:to_binary(K), maps:get(value, V, "")}
    end, M),
    maps:from_list(L1).

to_model(D1, M) ->
    D = [{ss:to_binary(K), V} || {K, V} <- maps:to_list(D1)],
    lists:map(fun({K, Field}) ->
        V = proplists:get_value(K, D, <<>>),
        {K, Field#{value => V}}
    end, confirm_model(M)).

