%% -*- mode: nitrogen -*-
-module(ss_server).
-behaviour(gen_server).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([info/1, model/2, connect/2, connect/3, validate/4]).
-export([dispatch/2]).

%% ss_server behaviour define ---------------------------------------
%%
%% init(_) -> {ok, #{db=>ss_nosqlite, res=>user}}.
%%
-type state() :: term().
-callback init(list()) -> {ok, state()}.
-callback model(atom()) -> list().

%% you must implement some method as follow:
%%
%% -callback call_method(state()) -> {result, state()}.
%% -callback call_method(req1().., state()) -> {result, state()}.
%% -callback cast_method(req1().., {pid(), reference()}, state()) -> {result, state()}.
%%

%% help module to start
%% the params must be a list and the first element is module
start_link(World, Mods, Args) ->
    gen_server:start_link(?MODULE, [World, Mods, Args], []).

%% gen_server api
%%

init([World, Mods, Args]) ->
    {Flag, S0} = case check_fun(Mods, init, 1) of
        not_found -> {ok, #{}};
        M -> apply(M, init, [Args])
    end,
    {Flag, S0#{world=>World, mods=>Mods}}.

%% delegate handle_call-----------------------------------------------
%%
%% give up _From and use an atom
handle_call({Fun, Args}, _From, #{mods:=Mods}=S1) ->
    {Result, S2} = handle_delegate(Mods, Fun, Args, S1),
    {reply, Result, S2}.

%% delegate handle_cast-----------------------------------------------
%%
%% use From for cast, you can send process message directly
handle_cast({Fun, Args}, #{mods:=Mods}=S1) ->
    % erlang:display("handle_cast [Fun|Args]"),
    % erlang:display(Mod),
    % erlang:display(Fun),
    % erlang:display(Args),
    {_Result, S2} = handle_delegate(Mods, Fun, Args, S1),
    {noreply, S2}.

%% give up follow methods
handle_info(undefined_info, S) ->  {noreply, S}.
terminate(normal, _S) -> ok.
code_change(undefined_oldVsn, S, _Extra) -> {ok, S}.

%% module info ----------------------------------------------------
%%
info(S) -> {S, S}.

%% query model ----------------------------------------------------
%%
model(Action, #{mods:=Mods}=S) ->
    {apply(check_fun(Mods, model, 1), model, [Action]), S}.

%% connect to model -----------------------------------------------
%%    for example, n2o websocket can receive message by messge slots
connect(Pid, S) -> connect(Pid, fun(Msg) -> Msg end, S).

%% 允许分发消息时使用模板
connect(Pid, TempFun, S) when is_pid(Pid) and is_function(TempFun, 1) ->
    Slots = maps:get(slots, S, []),
    case lists:keymember(Pid, 1, Slots) of
        true ->  {{ok, alread_connected}, S};
        false -> {{ok, connected}, S#{slots=>[{Pid, TempFun}|Slots]}}
    end.

%% 分发消息
dispatch(Msg, S) ->
    Slots = maps:get(slots, S, []),
    [Pid ! Fun(Msg) || {Pid, Fun} <- Slots].

%% private ------------------------------------------------------------
%%

%% check function in mods
check_fun([], _Fun, _Argc) -> not_found;
check_fun([Mod|Rest], Fun, Argc) ->
    case erlang:function_exported(Mod, Fun, Argc) of
        true -> Mod;
        false -> check_fun(Rest, Fun, Argc)
    end.

%% delegate action to module or do it self
%% force to check the return type
handle_delegate(Mods, Fun, Args, S) ->
    case check_fun(Mods, Fun, length(Args)+1) of
        not_found ->
            case erlang:function_exported(?MODULE, Fun, length(Args)+1) of
                true -> {_Result, #{world:=_, mods:=_, db:=_}} = apply(?MODULE, Fun, Args++[S]);
                false -> {{error, no_action, [Mods, Fun, Args]}, S}
            end;
        Mod -> {_Result, #{world:=_, mods:=_, db:=_}} = apply(Mod, Fun, Args++[S])
    end.

%% validate for create and update to db
validate(Data, M0, #{mods:=Mods}=S, Fun) ->
    if
        is_atom(M0) ->
            case check_fun(Mods, model, 1) of
                not_found -> exit({error, not_found, model, M0, in, Mods}), M1=not_found;
                Mod -> M1 = apply(Mod, model, [M0])
            end;
        true -> M1 = M0
    end,
    M2 = ss_model:to_model(Data, M1),
    case ss_validate:check(M2, S) of
        {ok, _} -> {Fun(), S};
        {error, M3} -> {{error, M3}, S}
    end.
