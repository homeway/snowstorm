%% -*- mode: nitrogen -*-
-module(ss_model).
-export([face/2, value/2, value/3, equal/3]).

%% get model
face(Face, Models) -> maps:get(Face, Models).

%% get value from model with key
value(Key, Model) ->
    V = proplists:get_value(Key, Model),
    maps:get(value, V).
value(Key, Model, Default) ->
    V = proplists:get_value(Key, Model),
    maps:get(value, V, Default).

%% equal to a existing value
equal(Key, Model, ToCompare) ->
    value(Key, Model, undefined) =:= ToCompare.
