%% -*- mode: nitrogen -*-
-module(ss_world).
-behaviour(gen_server).

%% common world api
-export([start_link2/1, start2/1, stop2/1]).
-export([all2/1, info2/2, clear2/1, destroy2/1, reg2/3, reg2/4, reg_server2/3, reg_server2/4, unreg2/2, find2/2, send2/3, call2/3]).

%% default world api
-export([start_link/0, start/0, stop/0]).
-export([all/0, info/1, clear/0, destroy/0, reg/2, reg/3, reg_server/2, reg_server/3, unreg/1, find/1, send/2, call/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% api for manager
start() -> start2(?SERVER).
stop() -> stop2(?SERVER).
start_link() -> start_link2(?SERVER).

start2(WorldName) -> gen_server:start({local, WorldName}, ?MODULE, [WorldName], []).
stop2(WorldName) -> gen_server:cast(WorldName, stop).
start_link2(WorldName) -> gen_server:start_link({local, WorldName}, ?MODULE, [WorldName], []).

%% @doc you can build some different world in the same application
%%
%% WN is WorldName
%% PN is ProcessName
%% Mod is Module
%%
reg2(WN, PN, Mod) -> reg2(WN, PN, Mod, []).
reg2(WN, PN, Mod, Args) when is_atom(Mod) and is_list(Args) ->
    gen_server:call(WN, {reg, PN, Mod, Args}).

reg_server2(WN, PN, Mod) -> reg_server2(WN, PN, Mod, []).
reg_server2(WN, PN, Mod, Args) when is_atom(Mod) and is_list(Args) ->
    gen_server:call(WN, {reg, PN, ss_server, [[Mod|Args]]}).

info2(WN, PN)      -> gen_server:call(WN, {info,  PN}).
unreg2(WN, PN)     -> gen_server:call(WN, {unreg, PN}).
send2(WN, PN, Msg) -> gen_server:call(WN, {send,  PN, Msg}).
call2(WN, PN, Req) -> gen_server:call(WN, {call,  PN, Req}).
find2(WN, PN)      -> gen_server:call(WN, {find,  PN}).
all2(WN)           -> gen_server:call(WN, all).
clear2(WN)         -> gen_server:call(WN, clear).
destroy2(WN)       -> gen_server:call(WN, destroy).

%% api with default name of ss_world
reg(PN, Mod)         -> reg2(?SERVER, PN, Mod, []).
reg(PN, Mod, Args)   -> reg2(?SERVER, PN, Mod, Args).
reg_server(PN, Mod)       -> reg_server2(?SERVER, PN, Mod, []).
reg_server(PN, Mod, Args) -> reg_server2(?SERVER, PN, Mod, Args).
info(PN)             -> info2(?SERVER, PN).
unreg(PN)            -> unreg2(?SERVER, PN).
send(PN, Msg)        -> send2(?SERVER, PN, Msg).
call(PN, Req)        -> call2(?SERVER, PN, Req).
find(PN)             -> find2(?SERVER, PN).
all()                -> all2(?SERVER).
clear()              -> clear2(?SERVER).
destroy()            -> destroy2(?SERVER).

%% start the models supervisor after the world start
init([WorldName|[]]) ->
    Str = io_lib:format("~p_sup", [WorldName]),
    WorldSup = list_to_atom(lists:flatten(Str)),
    ss_server_sup:start_link(WorldSup),
    {ok, #{world=>WorldSup}}.

%% reg model process with a uniq name
handle_call({reg, Name, Mod, Args}, _From, #{world:=World}=S0) ->
    Reply = case ss_server_sup:start_child(World, Name, Mod, Args) of
        {ok, Pid} -> Pid;
        {error,{already_started, Pid}} -> Pid;
        Error -> Error
    end,
    {reply, Reply, S0};

%% unreg process with uniq name
handle_call({unreg, Name}, _From, #{world:=World}=S0) ->
    supervisor:terminate_child(World, Name),
    R = supervisor:delete_child(World, Name),
    {reply, R, S0};

%% find model process with uniq name
handle_call({find, Name}, _From, #{world:=World}=S0) ->
    S = supervisor:which_children(World),
    case lists:keyfind(Name, 1, S) of
        false -> {reply, false, S0};
        {Name, Pid, _, _} -> {reply, Pid, S0}
    end;

%% call handle_call in model process
handle_call({call, Name, Req}, _From, #{world:=World}=S0) ->
    S = supervisor:which_children(World),
    case lists:keyfind(Name, 1, S) of
        false -> {reply, false, S0};
        {Name, Pid, _, _} -> {reply, gen_server:call(Pid, Req), S0}
    end;

%% list all process in ss_world
handle_call(all, _From, #{world:=World}=S0) ->
    S = supervisor:which_children(World),
    {reply, [{Name, Pid} || {Name, Pid, _, _} <- S], S0};

%% list the process info with uniq name
handle_call({info, Name}, _From, #{world:=World}=S0) ->
    S = supervisor:which_children(World),
    case lists:keyfind(Name, 1, S) of
        false -> {reply, false, S0};
        {Name, Pid, _, _} -> {reply, erlang:process_info(Pid), S0}
    end;

%% clear undefined process in ss_world
handle_call(clear, _From, #{world:=World}=S0) ->
    OldS = supervisor:which_children(World),
    [supervisor:delete_child(World, Name) || {Name, Pid, _, _} <- OldS, Pid =:= undefined],
    NewS = supervisor:which_children(World),
    {reply, length(OldS) - length(NewS), S0};

%% clear undefined process in ss_world
handle_call(destroy, _From, #{world:=World}=S0) ->
    OldS = supervisor:which_children(World),
    lists:foreach(fun({Name, _, _, _}) ->
        supervisor:terminate_child(World, Name),
        supervisor:delete_child(World, Name)
    end, OldS),
    NewS = supervisor:which_children(World),
    {reply, length(OldS) - length(NewS), S0};

%% cast message to process
handle_call({send, Name, Msg}, {From, _}, #{world:=World}=S0) ->
    S = supervisor:which_children(World),
    R = case lists:keyfind(Name, 1, S) of
        false -> not_reg;
        {Name, Pid, _, _} -> gen_server:cast(Pid, {Msg, From})
    end,
    {reply, R, S0}.

handle_cast(stop, S) -> {stop, normal, S}.
handle_info(undefined_info, S) -> {noreply, S}.
terminate(normal, _S) -> ok.
code_change(undefined_oldVsn, S, _Extra) -> {ok, S}.