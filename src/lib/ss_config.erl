%% -*- mode: nitrogen -*-
-module(ss_config).
-export([name/1]).

%% @doc 支持在config模块中配置可替换项
name(ModuleName) ->
    case erlang:function_exported(config, ss_name, 1) of
        true ->
            try config:ss_name(ModuleName) of
                undefined -> ModuleName;
                ReplaceName -> ReplaceName
            catch
                _:_ -> ModuleName
            end;
        _ -> ModuleName
    end.