%% -*- mode: nitrogen -*-
-module(ss).
-compile(export_all).

%% world api ------------------------------------------------
world() -> {ss_world, snowstorm_world}.
world(Name) -> {ss_world, Name}.

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

%% world ----------------------------------------------------
% -define(SERVER, snowstorm_world).

% %%
% start() -> ss_world:start(?SERVER).
% stop() -> ss_world:stop(?SERVER).
% reg(PN, Mod)         -> ss_world:reg(?SERVER, PN, Mod, []).
% reg(PN, Mod, Args)   -> ss_world:reg(?SERVER, PN, Mod, Args).
% reg_server(PN, Mod)       -> ss_world:reg_server(?SERVER, PN, Mod, []).
% reg_server(PN, Mod, Args) -> ss_world:reg_server(?SERVER, PN, Mod, Args).
% info(PN)             -> ss_world:info(?SERVER, PN).
% unreg(PN)            -> ss_world:unreg(?SERVER, PN).
% send(PN, Msg)        -> ss_world:send(?SERVER, PN, Msg).
% call(PN, Req)        -> ss_world:call(?SERVER, PN, Req).
% find(PN)             -> ss_world:find(?SERVER, PN).
% all()                -> ss_world:all(?SERVER).
% clear()              -> ss_world:clear(?SERVER).
% destroy()            -> ss_world:destroy(?SERVER).

% %%
% start2(World) -> ss_world:start2(World).
% stop2(World) -> ss_world:stop2(World).
% reg2(World, PN, Mod)         -> ss_world:reg2(World, PN, Mod, []).
% reg2(World, PN, Mod, Args)   -> ss_world:reg2(World, PN, Mod, Args).
% reg_server2(World, PN, Mod)       -> ss_world:reg_server2(World, PN, Mod, []).
% reg_server2(World, PN, Mod, Args) -> ss_world:reg_server2(World, PN, Mod, Args).
% info2(World, PN)             -> ss_world:info2(World, PN).
% unreg2(World, PN)            -> ss_world:unreg2(World, PN).
% send2(World, PN, Msg)        -> ss_world:send2(World, PN, Msg).
% call2(World, PN, Req)        -> ss_world:call2(World, PN, Req).
% find2(World, PN)             -> ss_world:find2(World, PN).
% all2(World)                -> ss_world:all2(World).
% clear2(World)              -> ss_world:clear2(World).
% destroy2(World)            -> ss_world:destroy2(World).
