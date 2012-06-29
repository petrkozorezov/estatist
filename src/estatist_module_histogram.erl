% @author Kozorezov Petr <petr.kozorezov@gmail.com>
% @copyright 2012 Kozorezov Petr
% @private
-module(estatist_module_histogram).
-behavour(estatist_module).

-export([
         init/2,
         get/3,
         update/3,
         tick/1,
         terminate/0
        ]).

-record(params, {size = 1000}).

init(_, Options) ->
    ParseOption =
        fun({size, V}, Params=#params{}) ->
                Params#params{size = V};
           (Opt, _) ->
                throw({?MODULE, unknown_option, Opt})
        end,
    Params = lists:foldr(ParseOption, #params{}, Options),

    {ok, Histogram} = basho_metrics_nifs:histogram_new([{size, Params#params.size}]),
    {Histogram, undefined}.

get(_, Histogram, all) ->
    basho_metrics_nifs:histogram_stats(Histogram);

get(_, Histogram, Params) when is_list(Params) ->
    Stats = basho_metrics_nifs:histogram_stats(Histogram),
    F = fun({Type, _}) ->
                lists:member(Type, Params)
        end,
    lists:filter(F, Stats);
get(_, Histogram, Param) ->
    Stats = basho_metrics_nifs:histogram_stats(Histogram),
    proplists:get_value(Param, Stats).

update(_, Histogram, Value) ->
    basho_metrics_nifs:histogram_update(Histogram, Value).

tick(_) ->
    ok.

terminate() ->
    ok.
