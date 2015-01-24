%% -*- mode: nitrogen -*-
-module(ss_world).
-behaviour(gen_server).

-export([start_link/0, start/0, stop/0]).
-export([list/0, list/1, live/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

list() -> [ss_user].
list(live) -> [ss_user].

live(Module, Name, Data) -> Module:start(Name, Data).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:cast(?SERVER, stop).

init([]) -> {ok, #{db=>ss_nosqlite}}.
handle_call({start, Module, Name, Data}, _From, State) ->
    {ok, Pid} = Module:start(Name, Data),
    {reply, ok, maps:put(Name, {Pid, Module}, State)};
handle_call({stop, Name}, _From, State) ->
    {_Pid, M} = maps:get(Name, State),
    M:stop(),
    {reply, ok, maps:remove(Name, State)};
handle_call(list, _From, State) ->
    {reply, State, State}.

handle_cast(stop, State) ->  {stop, normal, State}.
handle_info(undefined_info, State) ->  {noreply, State}.
terminate(normal, _State) -> ok.
code_change(undefined_oldVsn, State, _Extra) -> {ok, State}.