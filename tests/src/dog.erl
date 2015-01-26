-module(dog).
-behaviour(gen_server).
-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() -> gen_server:start_link(?MODULE, [], []).
start_link(Color) -> gen_server:start_link(?MODULE, [Color], []).

init([Color]) -> {ok, #{color=>Color}};
init(_) -> {ok, #{color=>yellow}}.

handle_call(hello, _From, #{color:=Color}=State) ->
    io:format("call: ~p, won! won! ~n", [Color]),
    {reply, ok, State}.

handle_cast({_From, hello}, #{color:=Color}=State) ->
    io:format("message: ~p, won! won! ~n", [Color]),
    {noreply, State}.

handle_info(undefined_info, State) ->  {noreply, State}.
terminate(normal, _State) -> ok.
code_change(undefined_oldVsn, State, _Extra) -> {ok, State}.