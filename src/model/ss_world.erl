%% -*- mode: nitrogen -*-
-module(ss_world).
-behaviour(gen_server).

-export([start_link/0, start/0, stop/0]).
-export([all/0, info/1, clear/0, reg/2, reg/3, unreg/1, find/1, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% api for manager
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:cast(?SERVER, stop).

%% api for user
all() -> gen_server:call(?SERVER, all).
info(Name) -> gen_server:call(?SERVER, {info, Name}).
clear() -> gen_server:call(?SERVER, clear).
reg(Name, Mod) -> reg(Name, Mod, []).
reg(Name, Mod, Args) -> gen_server:call(?SERVER, {reg, Name, Mod, Args}).
unreg(Name) -> gen_server:call(?SERVER, {unreg, Name}).
send(Name, Msg) -> gen_server:call(?SERVER, {send, Name, Msg}).
find(Name) -> gen_server:call(?SERVER, {find, Name}).

%% start the models supervisor after the world start
init([]) ->
    ss_model_sup:start_link(),
    NewS = supervisor:which_children(ss_model_sup),
    {ok, NewS}.

%% reg model process with a uniq name
handle_call({reg, Name, Mod, Args}, _From, _S) ->
    R = ss_model_sup:start_child(Name, Mod, Args),
    NewS = supervisor:which_children(ss_model_sup),
    {reply, R, NewS};
handle_call({unreg, Name}, _From, _S) ->
    R = supervisor:delete_child(ss_model_sup, Name),
    NewS = supervisor:which_children(ss_model_sup),
    {reply, R, NewS};
handle_call({find, Name}, _From, S) ->
    case lists:keyfind(Name, 1, S) of
        false -> {reply, false, S};
        {Name, Pid, _, _} -> {reply, Pid, S}
    end;
handle_call({send, Name, Msg}, _From, S) ->
    case lists:keyfind(Name, 1, S) of
        false -> {reply, false, S};
        {Name, Pid, _, _} -> {reply, gen_server:call(Pid, Msg), S}
    end;
handle_call(all, _From, S) ->
    {reply, [{Name, Pid} || {Name, Pid, _, _} <- S], S};
handle_call({info, Name}, _From, S) ->
    case lists:keyfind(Name, 1, S) of
        false -> {reply, false, S};
        {Name, Pid, _, _} -> {reply, erlang:process_info(Pid), S}
    end;
handle_call(clear, _From, _S) ->
    OldS = supervisor:which_children(ss_model_sup),
    [supervisor:delete_child(ss_model_sup, Name) || {Name, Pid, _, _} <- OldS, Pid =:= undefined],
    NewS = supervisor:which_children(ss_model_sup),
    {reply, length(OldS) - length(NewS), NewS}.

handle_cast(stop, S) -> {stop, normal, S}.
handle_info(undefined_info, S) -> {noreply, S}.
terminate(normal, _S) -> ok.
code_change(undefined_oldVsn, S, _Extra) -> {ok, S}.
