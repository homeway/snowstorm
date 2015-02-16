%% -*- mode: nitrogen -*-
-module(my_account).
-export([who/1]).

who(#{account:=Account}=S) -> {{custom, Account}, S}.
