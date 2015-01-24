%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2015, homeway
%%% @doc
%%% scaffold for ss theme
%%% @end
%%% Created :  2 Jan 2015 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(ss_scaffold).
-export([build/3]).

%% @doc 构建文件
build(Src, Dest, Options) ->
    case filelib:is_file(Src) of
        true ->
            io:format("scaffold -> [ok] from\t: ~s\n\r", [Src]),
            case filelib:is_file(Dest) of
                true ->
                    io:format("scaffold -> [already exist] to\t: ~s\n\r", [Dest]);
                false ->
                    io:format("scaffold -> [~p] to\t: ~s\n\r", [render(Src, Dest, Options), Dest])
            end;
        false ->
            io:format("scaffold -> [not exist] from: ~s\n\r", [Src])
    end,
    ok.

render(Src, Dest, Options) ->
    {ok, B} = file:read_file(Src),
    Content = nitrogen_render_template(B, Options),
    %io:format("~ts", [Content]),
    erlang:display(file:write_file(Dest, pp:to_binary(Content))),
    ok.

%% @doc nitrogen风格的模板替换
%% @end
nitrogen_render_template(B1, Options1) ->
    S = unicode:characters_to_list(B1),
    Options = [{ss:to_list(K), ss:to_list(V)} || {K, V}<-Options1],
    replace_name(S, Options).

replace_name(S, Options) ->
    case S of
        "[[[" ++ Rest -> 
            replace_name_trip(Rest, Options, "");
        [H|T] ->
            [H|replace_name(T, Options)];
        [] ->
            []
    end.

replace_name_trip(S, Options, Acc) ->
    case S of
        "]]]" ++ Rest ->
            K = lists:reverse(Acc),
            V = proplists:get_value(K, Options, ""),
            [V|replace_name(Rest, Options)];
        [H|T] ->
            replace_name_trip(T, Options, [H|Acc]);
        [] ->
            io:format("failed to replace template: ~ts", [lists:reverse(Acc)])
    end.
