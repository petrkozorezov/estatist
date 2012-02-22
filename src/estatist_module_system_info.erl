-module(estatist_module_system_info).
-behavour(metrics).

-export([
         init/2,
         get/3,
         update/3,
         tick/1,
         terminate/0
        ]).

init(_, []) ->
    {undefined, undefined}.

get(_, _, all_params) ->
    get_all();

get(_, _, Params) when is_list(Params) ->
    Stats = get_all(),
    F = fun({Type, _}) ->
                lists:member(Type, Params)
        end,
    lists:filter(F, Stats);
get(_, _, Param) ->
    Stats = get_all(),
    proplists:get_value(Param, Stats).

update(_, _, _) ->
    throw(not_allowed).

tick(_) ->
    ok.

terminate() ->
    ok.

get_all() ->
    [{Type, erlang:system_info(Type)} || Type <- [logical_processors_online, process_count, process_limit, thread_pool_size, creation]].
