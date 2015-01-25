-module(dog).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() -> gen_server:start_link(?MODULE, [], []).

init([]) -> {ok, #{color=>yellow}}.
handle_call(hello, _From, State) ->
    io:format("call: won! won! ~n"),
    {reply, ok, State}.

handle_cast({_From, hello}, State) ->
    io:format("message: won! won! ~n"),
    {noreply, State}.

handle_info(undefined_info, State) ->  {noreply, State}.
terminate(normal, _State) -> ok.
code_change(undefined_oldVsn, State, _Extra) -> {ok, State}.