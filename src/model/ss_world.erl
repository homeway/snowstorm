%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2015, homeway
%%% @doc
%%% Start2(WorldName) will start ss_world and ss_server_sup
%%%   with registered name  WorldName and {WorldName}_sup
%%%
%%% Start() is a fast method of start(?MODULE).
%%%
%%% You can register an otp process to ss_world and ss_server_sup,
%%% but I encouraged you register an ss_server process. There are many useful
%%% feature with ss_server such as crud of database.
%%%
%%% @end
%%% Created : 28 Jan 2015 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(ss_world).
-behaviour(gen_server).

%% common world api
-export([start_link/1, start/1, stop/1]).
-export([all/1, info/2, clear/1, destroy/1, find/2, send/3, call/3,
    reg/3, reg/4, reg_server/3, reg_server/4, unreg/2]).

%% default world api
% -export([start_link/0, start/0, stop/0]).
% -export([all/0, info/1, clear/0, destroy/0, find/1, send/2, call/2,
%     reg/2, reg/3, reg_server/2, reg_server/3, unreg/1]).

%% gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, snowstorm_world).

%% api for manager
% start() -> start2(?SERVER).
% stop() -> stop2(?SERVER).
% start_link() -> start_link2(?SERVER).

start({?MODULE, WN}) -> start(WN);
start(WorldName) ->
    gen_server:start({local, WorldName}, ?MODULE, [WorldName], []),
    {?MODULE, WorldName}.

stop({?MODULE, WorldName}) -> stop(WorldName);
stop(WorldName) -> gen_server:cast(WorldName, stop).
start_link(WorldName) -> gen_server:start_link({local, WorldName}, ?MODULE, [WorldName], []).

%% @doc you can build some different world in the same application
%%
%% WN is WorldName
%% PN is ProcessName
%% Mod is Module
%%
reg(PN, Mod, {?MODULE, WN}) -> reg(WN, PN, Mod);
reg(WN, PN, Mod) -> reg(WN, PN, Mod, []).

reg(PN, Mod, Args, {?MODULE, WN}) -> reg(WN, PN, Mod, Args);
reg(WN, PN, Mod, Args) when is_atom(Mod) and is_list(Args) ->
    gen_server:call(WN, {reg, PN, Mod, Args}).

reg_server(PN, Mod, {?MODULE, WN}) -> reg_server(WN, PN, Mod);
reg_server(WN, PN, Mod) -> reg_server(WN, PN, Mod, []).

reg_server(PN, Mod, Args, {?MODULE, WN}) -> reg_server(WN, PN, Mod, Args);
reg_server(WN, PN, Mod, Args) when is_atom(Mod) and is_list(Args) ->
    gen_server:call(WN, {reg, PN, ss_server, [[Mod|Args]]}).

info(PN, {?MODULE, WN}) -> info(WN, PN);
info(WN, PN)      -> gen_server:call(WN, {info,  PN}).

unreg(PN, {?MODULE, WN}) -> unreg(WN, PN);
unreg(WN, PN)     -> gen_server:call(WN, {unreg, PN}).

call(PN, Req, {?MODULE, WN}) -> call(WN, PN, Req);
call(WN, PN, Req) -> gen_server:call(WN, {call,  PN, Req}).

find(PN, {?MODULE, WN}) -> find(WN, PN);
find(WN, PN)      -> gen_server:call(WN, {find,  PN}).

all({?MODULE, WN}) -> all(WN);
all(WN)           -> gen_server:call(WN, all).

clear({?MODULE, WN}) -> clear(WN);
clear(WN)         -> gen_server:call(WN, clear).

destroy({?MODULE, WN}) -> destroy(WN);
destroy(WN)       -> gen_server:call(WN, destroy).

send(PN, Msg, {?MODULE, WN}) -> send(WN, PN, Msg);
send(WN, PN, Msg) -> gen_server:cast(WN, {send,  PN, Msg}).

%% api with default name of ss_world
% reg(PN, Mod)         -> reg2(?SERVER, PN, Mod, []).
% reg(PN, Mod, Args)   -> reg2(?SERVER, PN, Mod, Args).
% reg_server(PN, Mod)       -> reg_server2(?SERVER, PN, Mod, []).
% reg_server(PN, Mod, Args) -> reg_server2(?SERVER, PN, Mod, Args).
% info(PN)             -> info2(?SERVER, PN).
% unreg(PN)            -> unreg2(?SERVER, PN).
% send(PN, Msg)        -> send2(?SERVER, PN, Msg).
% call(PN, Req)        -> call2(?SERVER, PN, Req).
% find(PN)             -> find2(?SERVER, PN).
% all()                -> all2(?SERVER).
% clear()              -> clear2(?SERVER).
% destroy()            -> destroy2(?SERVER).

%% start the models supervisor after the world start
init([World|[]]) ->
    Str = io_lib:format("~p_sup", [World]),
    WorldSup = list_to_atom(lists:flatten(Str)),
    ss_server_sup:start_link(WorldSup),
    {ok, #{world=>World, world_sup=>WorldSup}}.

%% reg model process with a uniq name
handle_call({reg, Name, Mod, [Args]}, _From, #{world:=World, world_sup:=WorldSup}=S0) ->
    Reply = case ss_server_sup:start_child(WorldSup, Name, Mod, [[World|Args]]) of
        {ok, Pid} -> Pid;
        {error,{already_started, Pid}} -> Pid;
        Error -> Error
    end,
    {reply, Reply, S0};

%% unreg process with uniq name
handle_call({unreg, Name}, _From, #{world_sup:=WorldSup}=S0) ->
    supervisor:terminate_child(WorldSup, Name),
    R = supervisor:delete_child(WorldSup, Name),
    {reply, R, S0};

%% find model process with uniq name
handle_call({find, Name}, _From, #{world_sup:=WorldSup}=S0) ->
    S = supervisor:which_children(WorldSup),
    case lists:keyfind(Name, 1, S) of
        false -> {reply, not_reg, S0};
        {Name, Pid, _, _} -> {reply, Pid, S0}
    end;

%% call handle_call in model process
handle_call({call, Name, Req}, _From, #{world_sup:=WorldSup}=S0) ->
    S = supervisor:which_children(WorldSup),
    case lists:keyfind(Name, 1, S) of
        false -> {reply, not_reg, S0};
        {Name, Pid, _, _} -> {reply, gen_server:call(Pid, Req), S0}
    end;

%% list all process in ss_world
handle_call(all, _From, #{world_sup:=WorldSup}=S0) ->
    S = supervisor:which_children(WorldSup),
    {reply, [{Name, Pid} || {Name, Pid, _, _} <- S], S0};

%% list the process info with uniq name
handle_call({info, Name}, _From, #{world_sup:=WorldSup}=S0) ->
    S = supervisor:which_children(WorldSup),
    case lists:keyfind(Name, 1, S) of
        false -> {reply, not_reg, S0};
        {Name, Pid, _, _} -> {reply, erlang:process_info(Pid), S0}
    end;

%% clear undefined process in ss_world
handle_call(clear, _From, #{world_sup:=WorldSup}=S0) ->
    OldS = supervisor:which_children(WorldSup),
    [supervisor:delete_child(WorldSup, Name) || {Name, Pid, _, _} <- OldS, Pid =:= undefined],
    NewS = supervisor:which_children(WorldSup),
    {reply, length(OldS) - length(NewS), S0};

%% clear undefined process in ss_world
handle_call(destroy, _From, #{world_sup:=WorldSup}=S0) ->
    OldS = supervisor:which_children(WorldSup),
    lists:foreach(fun({Name, _, _, _}) ->
        supervisor:terminate_child(WorldSup, Name),
        supervisor:delete_child(WorldSup, Name)
    end, OldS),
    NewS = supervisor:which_children(WorldSup),
    {reply, length(OldS) - length(NewS), S0};

%% not support action
handle_call(_, _, S) ->
    {reply, {error, not_support}, S}.

%% cast message to process
handle_cast({send, Name, Msg}, #{world_sup:=WorldSup}=S0) ->
    S = supervisor:which_children(WorldSup),
    case lists:keyfind(Name, 1, S) of
        {Name, Pid, _, _} -> gen_server:cast(Pid, Msg);
        _ -> not_cast
    end,
    {noreply, S0};
handle_cast(stop, S) -> {stop, normal, S}.

handle_info(undefined_info, S) -> {noreply, S}.
terminate(normal, _S) -> ok.
code_change(undefined_oldVsn, S, _Extra) -> {ok, S}.
