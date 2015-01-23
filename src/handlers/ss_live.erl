%% -*- mode: nitrogen -*-
-module(ss_live).
-behaviour(gen_server).
-export([start/0, stop/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {}).

start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:cast(?SERVER, stop).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) -> {ok, #state{}}.
handle_call(_Msg, _From, State) -> {reply, ok, State}.
handle_cast(stop, State) ->  {stop, normal, State}.
handle_info(undefined_info, State) ->  {noreply, State}.
terminate(normal, _State) -> ok.
code_change(undefined_oldVsn, State, _Extra) -> {ok, State}.