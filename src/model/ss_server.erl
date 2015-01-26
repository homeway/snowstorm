%% -*- mode: nitrogen -*-
-module(ss_server).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([info/1, model/2, create/3, update/3, delete/2, find/2, all/1]).

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
start_link([Mod|Args]) -> gen_server:start_link(?MODULE, [Mod|Args], []).

%% gen_server api
%%

init([Mod|Args]) ->
    {Flag, S0} = apply(Mod, init, [Args]),
    {Flag, S0#{mod=>Mod}}.

%% delegate handle_call-----------------------------------------------
%%
%% give up _From and use an atom
handle_call(Fun, _From, #{mod:=Mod}=S1) when is_atom(Fun) ->
    {Result, S2} = handle_delegate(Mod, Fun, [], S1),
    {reply, Result, S2};

%% give up _From and use params
handle_call([Fun|Args], _From, #{mod:=Mod}=S1) ->
    {Result, S2} = handle_delegate(Mod, Fun, Args, S1),
    {reply, Result, S2}.

%% delegate handle_cast-----------------------------------------------
%%
%% use From for cast, you can send process message directly
handle_cast({[Fun|Args], From}, #{mod:=Mod}=S1) ->
    {_Result, S2} = handle_delegate(Mod, Fun, Args, From, S1),
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
model(Action, #{mod:=Mod}=S) -> {apply(Mod, model, [Action]), S}.

%% db action ------------------------------------------------------
%% Data is a db map #{Key=>Value}
create(Data, M, #{db:=Db, res:=Res}=S) ->
    case ss_model:validate(ss_model:confirm_model(M)) of
        {ok, _} -> {Db:create(Res, Data), S};
        {error, M2} -> {{error, M2}, S}
    end.

update(Data, M, #{db:=Db, res:=Res}=S) ->
    case ss_model:validate(ss_model:confirm_model(M)) of
        {ok, _} -> {Db:patch(Res, Data), S};
        {error, M2} -> {{error, M2}, S}
    end.

delete(K, #{db:=Db, res:=Res}=S) ->
    {apply(Db, delete, [Res, K]), S}.

find(K, #{db:=Db, res:=Res}=S) ->
    {apply(Db, find, [Res, K]), S}.

all(#{db:=Db, res:=Res}=S) ->
    {apply(Db, all, [Res]), S}.

%% private ------------------------------------------------------------
%%
%% delegate action to module or do it self
handle_delegate(Mod, Fun, Args, S) ->
    case erlang:function_exported(Mod, Fun, length(Args)+1) of
        true -> apply(Mod, Fun, Args++[S]);
        false ->
            case erlang:function_exported(?MODULE, Fun, length(Args)+1) of
                true -> apply(?MODULE, Fun, Args++[S]);
                false -> {{error, no_action, [Mod, Fun, Args]}, S}
            end
    end.
handle_delegate(Mod, Fun, Args, From, S) ->
    case erlang:function_exported(Mod, Fun, length(Args)+1) of
        true -> apply(Mod, Fun, Args++[From, S]);
        false ->
            case erlang:function_exported(?MODULE, Fun, length(Args)+1) of
                true -> apply(?MODULE, Fun, Args++[From, S]);
                false -> {{error, no_action, [Mod, Fun, Args]}, S}
            end
    end.