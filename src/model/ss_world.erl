%% -*- mode: nitrogen -*-
-module(ss_world).
-behaviour(gen_server).

-export([start_link/0, start/0, stop/0]).
-export([all/0, info/1, clear/0, reg/2, reg/3, regss/3, unreg/1, find/1, send/2, call/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% api for manager
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:cast(?SERVER, stop).

%% api for user
reg(Name, Mod)       -> reg(Name, Mod, []).
reg(Name, Mod, Args) -> gen_server:call(?SERVER, {reg, Name, Mod, Args}).
regss(Name, Mod, Args) -> gen_server:call(?SERVER, {reg, Name, ss_server, [[Mod|Args]]}).
info(Name)           -> gen_server:call(?SERVER, {info, Name}).
unreg(Name)          -> gen_server:call(?SERVER, {unreg, Name}).
send(Name, Msg)      -> gen_server:call(?SERVER, {send, Name, Msg}).
call(Name, Req)      -> gen_server:call(?SERVER, {call, Name, Req}).
find(Name)           -> gen_server:call(?SERVER, {find, Name}).
all()                -> gen_server:call(?SERVER, all).
clear()              -> gen_server:call(?SERVER, clear).

%% start the models supervisor after the world start
init([]) ->
    ss_model_sup:start_link(),
    {ok, #{}}.

%% reg model process with a uniq name
handle_call({reg, Name, Mod, Args}, _From, S0) ->
    Reply = case ss_model_sup:start_child(Name, Mod, Args) of
        {ok, Pid} -> Pid;
        {error,{already_started, Pid}} -> Pid;
        Error -> Error
    end,
    {reply, Reply, S0};

%% unreg process with uniq name
handle_call({unreg, Name}, _From, S0) ->
    R = supervisor:delete_child(ss_model_sup, Name),
    {reply, R, S0};

%% find model process with uniq name
handle_call({find, Name}, _From, S0) ->
    S = supervisor:which_children(ss_model_sup),
    case lists:keyfind(Name, 1, S) of
        false -> {reply, false, S0};
        {Name, Pid, _, _} -> {reply, Pid, S0}
    end;

%% call handle_call in model process
handle_call({call, Name, Req}, _From, S0) ->
    S = supervisor:which_children(ss_model_sup),
    case lists:keyfind(Name, 1, S) of
        false -> {reply, false, S0};
        {Name, Pid, _, _} -> {reply, gen_server:call(Pid, Req), S0}
    end;

%% list all process in ss_world
handle_call(all, _From, S0) ->
    S = supervisor:which_children(ss_model_sup),
    {reply, [{Name, Pid} || {Name, Pid, _, _} <- S], S0};

%% list the process info with uniq name
handle_call({info, Name}, _From, S0) ->
    S = supervisor:which_children(ss_model_sup),
    case lists:keyfind(Name, 1, S) of
        false -> {reply, false, S0};
        {Name, Pid, _, _} -> {reply, erlang:process_info(Pid), S0}
    end;

%% clear undefined process in ss_world
handle_call(clear, _From, S0) ->
    OldS = supervisor:which_children(ss_model_sup),
    [supervisor:delete_child(ss_model_sup, Name) || {Name, Pid, _, _} <- OldS, Pid =:= undefined],
    NewS = supervisor:which_children(ss_model_sup),
    {reply, length(OldS) - length(NewS), S0};

%% cast message to process
handle_call({send, Name, Msg}, {From, _}, S0) ->
    S = supervisor:which_children(ss_model_sup),
    R = case lists:keyfind(Name, 1, S) of
        false -> not_reg;
        {Name, Pid, _, _} -> gen_server:cast(Pid, {Msg, From})
    end,
    {reply, R, S0}.

handle_cast(stop, S) -> {stop, normal, S}.
handle_info(undefined_info, S) -> {noreply, S}.
terminate(normal, _S) -> ok.
code_change(undefined_oldVsn, S, _Extra) -> {ok, S}.