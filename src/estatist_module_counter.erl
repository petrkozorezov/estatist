-module(estatist_module_counter).
-behavour(estatist_module).

-export([
         init/2,
         get/3,
         update/3,
         tick/1,
         terminate/0
        ]).

init(Name, []) ->
    case ets:info(?MODULE) of
        undefined ->
            ets:new(?MODULE, [set, public, named_table]);
        _ ->
            ok
    end,
    true = ets:insert_new(?MODULE, {Name, 0}),
    {undefined, undefined}.

get(Name, Type, Params) when (Params =:= all_params) or (Params =:= [count]) ->
    [{count, get(Name, Type, count)}];
get(Name, _, count) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            undefined;
        [{Name, Val}] ->
            Val
    end;
get(_, _, []) ->
    [];
get(_, _, _) ->
    undefined.

update(Name, _, Value) ->
    ets:update_counter(?MODULE, Name, {2, Value}).

tick(_) ->
    ok.

terminate() ->
    ok.
