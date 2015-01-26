%% -*- mode: nitrogen -*-
-module(ss_server).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type state() :: term().
-callback init(list()) -> {ok, state()}.
-callback db() -> atom().
-callback resource() -> atom().
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
    {Flag, {Mod, S0}}.

%% module info
handle_call(info, _From, {Mod, S}) ->
    {reply, {Mod, S}, {Mod, S}};
%% query model
handle_call([model, Action], _From, {Mod, S}) ->
    {reply, apply(Mod, model, [Action]), {Mod, S}};
%% db action
handle_call([create, M], _From, {Mod, S}) ->
    {reply, apply(Mod:db(), create, [Mod:resource(), M]), {Mod, S}};
handle_call([update, M], _From, {Mod, S}) ->
    {reply, apply(Mod:db(), update, [Mod:resource(), M]), {Mod, S}};
handle_call([delete, K], _From, {Mod, S}) ->
    {reply, apply(Mod:db(), delete, [Mod:resource(), K]), {Mod, S}};
handle_call([find, K], _From, {Mod, S}) ->
    {reply, apply(Mod:db(), find, [Mod:resource(), K]), {Mod, S}};
handle_call(all, _From, {Mod, S}) ->
    {reply, apply(Mod:db(), all, [Mod:resource()]), {Mod, S}};
%% give up _From and use an atom
handle_call(Fun, _From, {Mod, S1}) when is_atom(Fun) ->
    {Result, S2} = apply(Mod, Fun, [S1]),
    {reply, Result, {Mod, S2}};
%% give up _From and use params
handle_call([Fun|Args], _From, {Mod, S1}) ->
    {Result, S2} = apply(Mod, Fun, Args++[S1]),
    {reply, Result, {Mod, S2}}.

%% use From for cast, you can send process message directly
handle_cast({[Fun|Args], From}, {Mod, S1}) ->
    {_Result, S2} = apply(Mod, Fun, Args++[From, S1]),
    {noreply, S2}.

%% give up follow methods
handle_info(undefined_info, S) ->  {noreply, S}.
terminate(normal, _S) -> ok.
code_change(undefined_oldVsn, S, _Extra) -> {ok, S}.