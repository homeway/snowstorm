%% -*- mode: nitrogen -*-
-module(ss_world_test).
-include_lib("eunit/include/eunit.hrl").

-define(world, test_world).
to_test() ->
    ss_world:start2(?world),
    ss_world:destroy2(?world),
    ?assertEqual([], ss_world:all2(?world)).
