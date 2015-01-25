%% -*- mode: nitrogen -*-
-module(ss_world).
-behaviour(gen_server).

-export([start_link/0, start/0, stop/0, info/0, reg/1, unreg/0, find/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:cast(?SERVER, stop).
info() -> gen_server:call(?SERVER, info).
reg(Name) -> gen_server:call(?SERVER, {reg, Name}).
unreg() -> gen_server:call(?SERVER, unreg).
find(Name) -> gen_server:call(?SERVER, {find, Name}).

%% start the models supervisor after the world start
init([]) ->
    ss_model_sup:start_link(),
    {ok, #{ets_tab=>ets:new(?SERVER, [])}}.

%% reg model process with a uniq name
handle_call({reg, Name}, {Pid, _Ref}, #{ets_tab:=Tab}=State) ->
    case ets:insert(Tab, {Pid, Name}) of
        true -> {reply, ok, State};
        _ -> {reply, failed, State}
    end;
%% unreg model process
handle_call(unreg, {Pid, _Ref}, #{ets_tab:=Tab}=State) ->
    erlang:display(ets:delete(Tab, Pid)),
    {reply, ok, State};
handle_call({find, Name}, _From, #{ets_tab:=Tab}=State) ->
    case ets:match(Tab, {'$1', Name}) of
        '$end_of_table' -> {reply, [], State};
        Pids -> {reply, [Pid||[Pid|[]] <- Pids], State}
    end;
handle_call(info, _From, #{ets_tab:=Tab}=State) ->
    {reply, all(Tab), State}.

handle_cast(stop, State) -> {stop, normal, State}.
handle_info(undefined_info, State) -> {noreply, State}.
terminate(normal, _State) -> ok.
code_change(undefined_oldVsn, State, _Extra) -> {ok, State}.

%% search all from ets
all(T) -> search_acc(T, ets:first(T), []).
search_acc(_T, '$end_of_table', Acc) -> Acc;
search_acc(T, K, Acc) ->
    [Obj|_] = ets:lookup(T, K),
    search_acc(T, ets:next(T, K), [Obj|Acc]).
