%% -*- mode: nitrogen -*-
-module([[[ResourceName]]]).
-behaviour(ss_server).
-export([init/1, model/1]).
-export([create/2, create/3, update/3, update/4, delete/2, find/2, all/1, drop/1]).

%% ss_server api
init([]) ->
    Default = #{db=>ss:db(ss_nosqlite, [[[ResourceName]]])},
    {ok, Default}.

model(all) -> ss_model:confirm_model([
    {"字段1", #{}},
    {"字段2", #{}},
    {"字段3", #{}}]);
model(create) -> model(all);
model(update) -> model(all);
model(_) -> [].

%% db action ------------------------------------------------------
create(Data, S) -> create(Data, create, S).
create(Data, Model, #{db:=D}=S) ->
    ss_server:validate(Data, Model, S, fun() ->
        D:create(Data)
    end).
update(K, Data, S) -> update(K, Data, update, S).
update(K, Data, Model, #{db:=D}=S) ->
    ss_server:validate(Data, Model, S, fun() ->
        D:update(K, Data)
    end).
delete(K, #{db:=D}=S) -> {D:delete(K), S}.
find  (K, #{db:=D}=S) -> {D:find(K), S}.
all      (#{db:=D}=S) -> {D:all(), S}.
drop     (#{db:=D}=S) -> {D:drop(), S}.

%% other action ----------------------------------------------------
