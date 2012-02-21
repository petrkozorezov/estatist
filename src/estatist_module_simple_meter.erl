-module(estatist_module_simple_meter).
-behavour(estatist_module).

-export([
         init/2,
         get/3,
         update/3,
         tick/1,
         terminate/0
        ]).

-record(params, {tick = 1000}).

init(_, Options) ->
    ParseOption =
        fun({tick, V}, Params=#params{}) ->
                Params#params{tick = V};
           (Opt, _) ->
                throw({?MODULE, unknown_option, Opt})
        end,
    Params = lists:foldr(ParseOption, #params{}, Options),

    {ok, Meter} = basho_metrics_nifs:meter_new([{tick_interval, Params#params.tick}]),
    {Meter, Params#params.tick}.


get(_, Meter, all_params) ->
    basho_metrics_nifs:meter_stats(Meter);

get(_, Meter, Params) when is_list(Params) ->
    Stats = basho_metrics_nifs:meter_stats(Meter),
    F = fun({Type, _}) ->
                lists:member(Type, Params)
        end,
    lists:filter(F, Stats);
get(_, Meter, Param) ->
    Stats = basho_metrics_nifs:meter_stats(Meter),
    proplists:get_value(Param, Stats).

update(_, Meter, _) ->
    basho_metrics_nifs:meter_update(Meter, 1).

tick(Meter) ->
    basho_metrics_nifs:meter_tick(Meter).

terminate() ->
    ok.
