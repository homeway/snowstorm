%% -*- mode: nitrogen -*-
-module(ss).
-compile(export_all).

%% world api ------------------------------------------------
world() -> {ss_world, snowstorm_world}.
world(Name) -> {ss_world, Name}.
server({ss_world, World}, Server) -> {ss_world, {World, Server}};
server(World, Server) -> {ss_world, {World, Server}}.

%% db api
db(Db, Res) -> {Db, Res}.
nosqlite(Res) -> {ss_nosqlite, Res}.

%% utils ----------------------------------------------------
to_binary(Term)     -> ss_convert:to_binary(Term).
to_list(Term)       -> ss_convert:to_list(Term).
to_atom(Term)       -> ss_convert:to_atom(Term).
print(Model)        -> ss_utils:print(Model).
url(T1)             -> ss_url:url(T1).
url(T1, T2)         -> ss_url:url(T1, T2).
url(T1, T2, T3)     -> ss_url:url(T1, T2, T3).
url(T1, T2, T3, T4) -> ss_url:url(T1, T2, T3, T4).

%% config ----------------------------------------------------
name(N) -> pp_config:name(N).