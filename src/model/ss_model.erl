%% -*- mode: nitrogen -*-
-module(ss_model).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([value/2, value/3, length/2, equal/3]).
-export([confirm_model/1, confirm_list/1, filter/2, drop/2]).
-export([validate/1, validate/2, required/2, max/3, min/3]).

%% get value from model with key
value(Key, Model) ->
    V = proplists:get_value(ss:to_binary(Key), Model),
    maps:get(value, V).
value(Key, Model, Default) ->
    V = proplists:get_value(ss:to_binary(Key), Model),
    maps:get(value, V, Default).

length(Key, Model) ->
    erlang:length(value(Key, Model, "")).

equal(Key, Model, ToCompare) ->
    value(Key, Model, undefined) =:= ToCompare.

%% convert model key to binary
confirm_model(M) -> [{ss:to_binary(K), V} || {K, V} <- M].
confirm_list(L) -> [ss:to_binary(K) || K <- L].

%% list filter and drop
filter(M, L1) ->
    L = confirm_list(L1),
    [{K, V} || {K, V} <- M, lists:any(fun(I) -> I =:= K end, L)].

drop(M, L1) ->
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
    validate(M, Errors).
%% merge all error message into model
validate(M, Errors1) ->
    Errors = lists:flatten(Errors1),
    if
        length(Errors) > 0 -> R = error;
        true -> R = ok
    end,
    {R, validate_acc(confirm_model(M), lists:flatten(Errors))}.

validate_acc(M0, []) -> M0;
validate_acc(M0, [{K, Error}|Rest]) ->
    Old = proplists:get_value(K, M0),
    New = Old#{error => Error},
    M1 = lists:keyreplace(K, 1, M0, {K, New}),
    validate_acc(M1, Rest).

required(K, M) ->
    case length(K, M) > 0 of
        true -> [];
        false ->
            Tip = <<"字段不能为空"/utf8>>,
            [{K, Tip}]
    end.
min(K, M, Len) ->
    case length(K, M) >= Len of
        true -> [];
        false ->
            Tip = <<"字段太短, 至少应为"/utf8, (ss:to_binary(Len))/binary, "位"/utf8>>,
            [{K, Tip}]
    end.
max(K, M, Len) ->
    case length(K, M) =< Len of
        true -> [];
        false ->
            Tip = <<"字段太长, 最多为"/utf8, (ss:to_binary(Len))/binary, "位"/utf8>>,
            [{K, Tip}]
    end.

%% gen_server default api
%% gen_server
start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, [], []).
start(Name) -> gen_server:start({local, Name}, ?MODULE, [], []).
start(Name, Model) -> gen_server:start({local, Name}, ?MODULE, Model, []).
stop(Name) -> gen_server:cast(Name, stop).
hello(Name) -> gen_server:call(Name, hello).

%% gen_server methods -------------------------------------------
init([]) -> {ok, #{}};
init(Model) when is_map(Model) -> {ok, Model}.

handle_call(hello, _From, State) ->
    io:format("hi!! ~n"),
    erlang:display(State),
    {reply, ok, State}.

handle_cast(stop, State) ->  {stop, normal, State}.
handle_info(_Info, State) ->  {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
