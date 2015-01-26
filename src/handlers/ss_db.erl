%% -*- mode:nitrogen -*-
-module(ss_db).

-type pos() :: term().
-type data() :: map().
-type id() :: binary().

-callback create(pos(), data()) -> {ok, id()}.
-callback create(pos(), id(), data()) -> {ok, id()}.
-callback find(pos(), id()) -> data().
-callback update(pos(), id(), data()) -> ok|{error, invalid_key}.
-callback patch(pos(), id(), data()) -> ok|{error, invalid_key}.
-callback delete(pos(), id()) -> ok.
-callback all(pos()) -> [data()].