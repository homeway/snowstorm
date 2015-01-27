%% -*- mode: nitrogen -*-
-module(ss_model).
-export([value/2, value/3, length/2, equal/3, set/2]).
-export([confirm_model/1, confirm_list/1, filter/2, drop/2]).
-export([validate/1, validate/2, custom_validate/3, required/2, max/3, min/3]).
-export([from_model/1, to_model/1, to_model/2]).

%% get value from model or map with key
%% model: [{K, map()}]
value(Key, Map) when is_map(Map) -> maps:get(ss:to_binary(Key), Map);
value(Key, Model) when is_list(Model) ->
    V = proplists:get_value(ss:to_binary(Key), Model),
    maps:get(value, V).

value(Key, Map, Default) when is_map(Map) -> maps:get(ss:to_binary(Key), Map, Default);
value(Key, Model, Default) ->
    V = proplists:get_value(ss:to_binary(Key), Model),
    maps:get(value, V, Default).

length(Key, Model) ->
    case value(Key, Model, <<>>) of
        <<>> -> 0;
        V -> erlang:length(V)
    end.

equal(Key, Model, ToCompare) ->
    value(Key, Model, undefined) =:= ToCompare.

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

%% validate model self
validate(M) ->
    Errors = lists:map(fun({K, Field}) ->
        Funs = maps:get(validate, Field, []),
        lists:map(fun(Fun) ->
            case Fun of
                required -> required(K, M);
                {min, Len} when is_integer(Len) -> min(K, M, Len);
                {max, Len} when is_integer(Len) -> max(K, M, Len);
                _ when is_function(Fun) -> Fun(K, M);
                {F, Args} when is_function(F) -> F(K, M, Args);
                {Module, F, A} when is_atom(Module) and is_atom(F) and is_list(A) -> apply(Module, F, [K, M|A])
            end
        end, Funs)
    end, M),
    validate(Errors, M).
%% merge all error message into model
validate(Errors1, M) ->
    Errors = lists:flatten(Errors1),
    if
        length(Errors) > 0 -> R = error;
        true -> R = ok
    end,
    M1 = validate_acc(lists:flatten(Errors), confirm_model(M)),
    {R, M1}.

validate_acc([], M0) -> M0;
validate_acc([{K, Error}|Rest], M0) ->
    Old = proplists:get_value(K, M0),
    New = Old#{error => Error},
    M1 = lists:keyreplace(K, 1, M0, {K, New}),
    validate_acc(Rest, M1).

%% validate required
custom_validate(K, Tip, Fun) when is_binary(K) and is_function(Fun) ->
    case Fun() of
        true -> [];
        false -> [{K, Tip}]
    end.
required(K, M) ->
    custom_validate(K, <<"字段不能为空"/utf8>>, fun() ->
        length(K, M) > 0
    end).

%% validate min length
min(K, M, Len) ->
    Tip = <<"字段太短, 至少应为"/utf8, (ss:to_binary(Len))/binary, "位"/utf8>>,
    custom_validate(K, Tip, fun() ->
        length(K, M) >= Len
    end).

%% validate max length
max(K, M, Len) ->
    Tip = <<"字段太长, 最多为"/utf8, (ss:to_binary(Len))/binary, "位"/utf8>>,
    custom_validate(K, Tip, fun() ->
        length(K, M) =< Len
    end).

%% convert model from db maps -------------------------------------
%% db maps: #{key() => value()}
from_model(M) ->
    L1 = lists:map(fun({K, V}) ->
        {ss:to_binary(K), maps:get(value, V, "")}
    end, M),
    maps:from_list(L1).

to_model(D) -> to_model(D, []).
to_model(D1, M) ->
    D = [{ss:to_binary(K), V} || {K, V} <- maps:to_list(D1)],
    lists:map(fun({K, Field}) ->
        V = proplists:get_value(K, D, <<>>),
        {K, Field#{value => V}}
    end, confirm_model(M)).

