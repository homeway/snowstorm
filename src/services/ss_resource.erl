%% -*- mode: nitrogen -*-
-module(ss_resource).
-behaviour(ss_server).
-export([init/1, model/1]).
-export([create/3, update/4, delete/2, find/2, all/1, drop/1]).

%% ss_server api
init([]) ->
    Default = #{db=>ss:db(ss_nosqlite, 'myresouce')},
    {ok, Default}.

model(all) -> ss_model:confirm_model([
    {"字段1", #{}},
    {"字段2", #{}},
    {"字段3", #{}}]);

model(_) -> [].

%% db action ------------------------------------------------------
create(Data, ModelName, #{db:=D}=S) ->
    case ss_validate:check(ss_model:to_model(Data, model(ModelName)), S) of
        {ok, _} -> {D:create(Data), S};
        {error, M3} -> {{error, M3}, S}
    end.

update(K, Data, ModelName, #{db:=D}=S) ->
    case ss_validate:check(ss_model:to_model(Data, model(ModelName)), S) of
        {ok, _} -> {D:update(K, Data), S};
        {error, M3} -> {{error, M3}, S}
    end.

delete(K, #{db:=D}=S) -> {D:delete(K), S}.
find  (K, #{db:=D}=S) -> {D:find(K), S}.
all      (#{db:=D}=S) -> {D:all(), S}.
drop     (#{db:=D}=S) -> {D:drop(), S}.