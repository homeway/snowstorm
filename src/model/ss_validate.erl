%% -*- mode: nitrogen -*-
-module(ss_validate).
-export([check/1, check/2, merge/2, custom/3]).
-export([required/2, max/3, min/3, int/2, list/2, select/2, uniq/3]).

%% validate model self
check(M) -> check(M, #{}).
check(M, S) ->
    Errors = lists:map(fun({K, Field}) ->
        Type = maps:get(type, Field, undefined),
        case lists:member(Type, [int, list, select]) of
            true ->
                Funs = [{?MODULE, Type, []}|maps:get(validate, Field, [])];
            false ->
                Funs = maps:get(validate, Field, [])
        end,
        lists:map(fun(Fun) ->
            case Fun of
                uniq -> uniq(K, Field, S);
                required -> required(K, Field);
                {min, Len} when is_integer(Len) -> min(K, Field, Len);
                {max, Len} when is_integer(Len) -> max(K, Field, Len);
                _ when is_function(Fun) -> Fun(K, Field);
                {F, Args} when is_function(F) -> F(K, Field, Args);
                {Module, F, A} when is_atom(Module) and is_atom(F) and is_list(A) -> apply(Module, F, [K, Field|A])
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

%% custom a validate function
custom(K, Tip, Fun) when is_binary(K) and is_function(Fun) ->
    case Fun() of
        true -> [];
        false -> [{K, ss:to_binary(Tip)}]
    end.

%% validate required
required(K, Field) ->
    custom(K, "field is required", fun() ->
        ss_model:length(Field) > 0
    end).

%% validate min length
min(K, Field, Len) ->
    Tip = <<"too short, need: "/utf8, (ss:to_binary(Len))/binary, " characters"/utf8>>,
    custom(K, Tip, fun() ->
        ss_model:length(Field) >= Len
    end).

%% validate max length
max(K, Field, Len) ->
    Tip = <<"too long, need: "/utf8, (ss:to_binary(Len))/binary, " characters"/utf8>>,
    custom(K, Tip, fun() ->
        ss_model:length(Field) =< Len
    end).

%% uniq field in db
%%
%% 1) create without id
%%    the value not exist
%%
%% 2) update with an id
%%    the value not exist; 
%%    or the value exist and bound with id in model
uniq(FName, Field, #{db:=Db, res:=Res}) ->
    V = ss_model:value(Field),
    custom(FName, "field must be uniq", fun() ->
        case Db:find(Res, FName, V) of
            notfound ->
                true;
            #{<<"_key">> := ExistKey} ->
                case ss_model:value(Field, undefined) of
                    ExistKey -> true;
                    _ -> false
                end
        end
    end);
uniq(K, _, _) ->
    custom(K, "not enough db info", fun() ->
        false
    end).

%% type check
int(K, Field) ->
    custom(K, "the value must be integer", fun() ->
        case maps:get(value, Field, <<>>) of
            <<>> -> true;
            V -> is_integer(V)
        end
    end).

list(K, Field) ->
    custom(K, "the value must be a list", fun() ->
        case maps:get(value, Field, <<>>) of
            <<>> -> true;
            V -> is_list(V)
        end
    end).

%% select
%% the value must belongs options when field type is select
%% example: [sex, #{type=> select, options=> [mail, femail]}]
select(K, Field) ->
    custom(K, "the value must be in select options", fun() ->
        case maps:get(value, Field, <<>>) of
            <<>> -> true;
            V ->
                Options = maps:get(options, Field, []),
                lists:member(V, Options)
        end
    end).
