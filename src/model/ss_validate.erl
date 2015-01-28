%% -*- mode: nitrogen -*-
-module(ss_validate).
-export([check/1, check/2, merge/2, custom/3]).
-export([required/2, max/3, min/3, uniq/3]).

%% validate model self
check(M) -> check(M, #{}).
check(M, S) ->
    Errors = lists:map(fun({K, Field}) ->
        Funs = maps:get(validate, Field, []),
        lists:map(fun(Fun) ->
            case Fun of
                uniq -> uniq(K, M, S);
                required -> required(K, M);
                {min, Len} when is_integer(Len) -> min(K, M, Len);
                {max, Len} when is_integer(Len) -> max(K, M, Len);
                _ when is_function(Fun) -> Fun(K, M);
                {F, Args} when is_function(F) -> F(K, M, Args);
                {Module, F, A} when is_atom(Module) and is_atom(F) and is_list(A) -> apply(Module, F, [K, M|A])
            end
        end, Funs)
    end, M),
    merge(Errors, M).
%% merge all error message into model
merge(Errors1, M) ->
    Errors = lists:flatten(Errors1),
    if
        length(Errors) > 0 -> R = error;
        true -> R = ok
    end,
    M1 = validate_acc(lists:flatten(Errors), ss_model:confirm_model(M)),
    {R, M1}.

validate_acc([], M0) -> M0;
validate_acc([{K, Error}|Rest], M0) ->
    Old = proplists:get_value(K, M0),
    New = Old#{error => Error},
    M1 = lists:keyreplace(K, 1, M0, {K, New}),
    validate_acc(Rest, M1).

%% validate required
custom(K, Tip, Fun) when is_binary(K) and is_function(Fun) ->
    case Fun() of
        true -> [];
        false -> [{K, Tip}]
    end.
required(K, M) ->
    custom(K, <<"字段不能为空"/utf8>>, fun() ->
        ss_model:length(K, M) > 0
    end).

%% validate min length
min(K, M, Len) ->
    Tip = <<"字段太短, 至少应为"/utf8, (ss:to_binary(Len))/binary, "位"/utf8>>,
    custom(K, Tip, fun() ->
        ss_model:length(K, M) >= Len
    end).

%% validate max length
max(K, M, Len) ->
    Tip = <<"字段太长, 最多为"/utf8, (ss:to_binary(Len))/binary, "位"/utf8>>,
    custom(K, Tip, fun() ->
        ss_model:length(K, M) =< Len
    end).

%% uniq field in db
%%
%% 1) create without id
%%    the value not exist
%%
%% 2) update with an id
%%    the value not exist; 
%%    or the value exist and bound with id in model
uniq(FName, M, #{db:=Db, res:=Res}) ->
    V = ss_model:value(FName, M),
    custom(FName, "field must be uniq", fun() ->
        case Db:find(Res, FName, V) of
            notfound -> true;
            #{<<"_key">> := ExistKey} -> ss_model:equal("_key", M, ExistKey);
            _ -> false
        end
    end);
uniq(K, _, _) ->
    custom(K, "not enough db info", fun() ->
        false
    end).