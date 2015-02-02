%% -*- mode: nitrogen -*-
-module(ss_server).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([info/1, model/2, connect/2, create/3, update/4, delete/2, find/2, all/1, drop/1]).

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
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

%% gen_server api
%%

init([World, Mod|Args]) ->
    {Flag, S0} = apply(Mod, init, [Args]),
    {Flag, S0#{world=>World, mod=>Mod}}.

%% delegate handle_call-----------------------------------------------
%%
%% give up _From and use an atom
handle_call(Fun, _From, #{mod:=Mod}=S1) when is_atom(Fun) ->
    {Result, S2} =  handle_delegate(Mod, Fun, [], S1),
    {reply, Result, S2};
handle_call([Fun|Args], _From, #{mod:=Mod}=S1) ->
    {Result, S2} = handle_delegate(Mod, Fun, Args, S1),
    {reply, Result, S2}.

%% delegate handle_cast-----------------------------------------------
%%
%% use From for cast, you can send process message directly
handle_cast(Fun, #{mod:=Mod}=S1) when is_atom(Fun) ->
    % erlang:display("handle_cast Fun"),
    % erlang:display(Mod),
    % erlang:display(Fun),
    {_Result, S2} = handle_delegate(Mod, Fun, [], S1),
    {noreply, S2};
handle_cast([Fun|Args], #{mod:=Mod}=S1) ->
    % erlang:display("handle_cast [Fun|Args]"),
    % erlang:display(Mod),
    % erlang:display(Fun),
    % erlang:display(Args),
    {_Result, S2} = handle_delegate(Mod, Fun, Args, S1),
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

%% connect to model -----------------------------------------------
%%    for example, n2o websocket can receive message by messge slots
connect(Pid, S) ->
    Slots = maps:get(slots, S, []),
    case lists:member(Pid, Slots) of
        true ->  {{ok, alread_connected}, S};
        false -> {{ok, connected}, S#{slots=>[Pid|Slots]}}
    end.

%% db action ------------------------------------------------------
%% Data is a db map #{Key=>Value}
create(Data, M0, #{db:=D}=S) ->
    validate(Data, M0, S, fun() ->
        D:create(Data)
    end).

update(K, Data, M0, #{db:=D}=S) ->
    validate(Data, M0, S, fun() ->
        D:update(K, Data)
    end).

delete(K, #{db:=D}=S) -> {D:delete(K), S}.
find  (K, #{db:=D}=S) -> {D:find(K), S}.
all      (#{db:=D}=S) -> {D:all(), S}.
drop     (#{db:=D}=S) -> {D:drop(), S}.

%% private ------------------------------------------------------------
%%
%% delegate action to module or do it self
%% force to check the return type
handle_delegate(Mod, Fun, Args, S) ->
    case erlang:function_exported(Mod, Fun, length(Args)+1) of
        true -> {_Result, #{world:=_, mod:=_, db:=_}} = apply(Mod, Fun, Args++[S]);
        false ->
            case erlang:function_exported(?MODULE, Fun, length(Args)+1) of
                true -> {_Result, #{world:=_, mod:=_, db:=_}} = apply(?MODULE, Fun, Args++[S]);
                false -> {{error, no_action, [Mod, Fun, Args]}, S}
            end
    end.

%% validate for create and update to db
validate(Data, M0, #{mod:=Mod}=S, Fun) ->
    if
        is_atom(M0) -> M1 = apply(Mod, model, [M0]);
        true        -> M1 = M0
    end,
    M2 = ss_model:to_model(Data, M1),
    case ss_validate:check(M2, S) of
        {ok, _} -> {Fun(), S};
        {error, M3} -> {{error, M3}, S}
    end.
