% @author Andrew Majorov <encube.ul@gmail.com>
% @copyright 2012 Andrew Majorov
% @private
-module(estatist_module_gauge).
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
            ?MODULE = ets:new(?MODULE, [set, public, named_table]);
        _ ->
            ok
    end,
    true = ets:insert_new(?MODULE, {Name, 0}),
    {undefined, undefined}.

get(Name, Type, Params) when (Params =:= all) or (Params =:= [value]) ->
    [{value, get(Name, Type, value)}];
get(Name, _, value) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            undefined;
        [{Name, Value}] ->
            Value
    end;
get(_, _, []) ->
    [];
get(_, _, _) ->
    undefined.

update(Name, _, Value) ->
    ets:insert(?MODULE, {Name, Value}).

tick(_) ->
    ok.

terminate() ->
    ok.
