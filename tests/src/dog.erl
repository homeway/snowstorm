-module(dog).
-behaviour(ss_server).
-export([init/1, handle_call/3]).

init([]) -> {ok, #{color=>yellow}}.
handle_call(hello, _From, State) ->
    io:format("won! won! ~n"),
    {reply, ok, State}.
