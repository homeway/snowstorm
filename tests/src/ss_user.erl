%% -*- mode: nitrogen -*-
-module(ss_user).
-behaviour(gen_server).
%% live
-export([start_link/0, start/0, start/2, hello/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([model/0, new/1, del/1, update/1, get/1, all/0, val/1]).

-define(SERVER, ?MODULE).

%% state
model() -> [
    {"类型", #{value=>"普通会员"}},
    {"账户名", #{value=>"yifan"}},
    {"性别", #{value=>"女"}},
    {"密码", #{type=>password, value=>"123456"}},
    {"EMail", #{}},
    {"电话", #{}},
    {"姓名", #{}},
    {"昵称", #{}},
    {"头像", #{type=>link, value=>"/themes/cube/img/samples/scarlet-159.png"}},
    {"生日", #{type=>date}},
    {"联系人", #{type=>tags}}
].

%% validate
%% 账户名和密码不能为空
%% 密码长度不能少于6位
from_model(M) ->
    L1 = lists:map(fun({K, V}) ->
        {ss:to_binary(K), maps:get(value, V, <<>>)}
    end, M),
    maps:from_list(L1).
val(M) ->
    Data = from_model(M),
    io:format("to verify..~n"),
    F1 = fun() -> length(maps:get(<<"账户名"/utf8>>, Data, "")) > 0 end,
    F2 = fun() -> length(maps:get(<<"密码"/utf8>>, Data, "")) >= 6 end,
    lists:all(fun(F) -> F() end, [F1, F2]).

%% db
db_create(M) -> {ok, _Id} = ss_nosqlite:create(user, M), true.
db_delete(M) -> ok =:= ss_nosqlite:delete(user, M).
db_update(M) -> ok =:= ss_nosqlite:patch(user, M).
db_get(Key)  -> ss_nosqlite:get(user, Key).
db_all()     -> ss_nosqlite:all(user).

%% exec handlers
go(Data, Handlers) ->
    lists:all(fun(Fn) ->
        Fn(Data)
    end, Handlers).
%% api new
new(Data) ->
    go(Data, [fun val/1, fun db_create/1]).
%% api delete
del(Data) ->
    go(Data, [fun db_delete/1]).
%% api update
update(Data) ->
    go(Data, [fun db_update/1]).
%% api get
get(Key) ->
    db_get(ss:to_binary(Key)).
%% api all
all() ->
    db_all().

%% gen_server
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).
start(Name, Data) -> gen_server:start({local, Name}, ?MODULE, Data, []).
hello() -> gen_server:call(?SERVER, hello).

stop() -> gen_server:cast(?SERVER, stop).

init([]) -> {ok, #{}};
init(Data) when is_map(Data) -> {ok, Data}.

handle_call(hello, _From, State) ->
    io:format("hi!! ~n"),
    erlang:display(State),
    {reply, ok, State}.

handle_cast(stop, State) ->  {stop, normal, State}.
handle_info(_Info, State) ->  {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
