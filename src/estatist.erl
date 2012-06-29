% @author Kozorezov Petr <petr.kozorezov@gmail.com>
% @copyright 2012 Kozorezov Petr
% @doc Fast application statistics agregator for erlang, based on basho_metrics.
-module(estatist).

-export_type([
    metric_type/0, metric_name/0, metric_type_param/0, metric_value/0, metric_scalarity/0,
    metric_type_options/0, metric_types/0, row_id/0, metric_id/0, metric_input_value/0
]).
-export_type([select_param/1, select_row_id/0, select_results/0, select_query/0]).
-export_type([app_options/0]).

-type metric_name()         :: atom() | string().
-type metric_input_value()  :: number().
-type metric_value()        :: metric_input_value() | undefined.
-type metric_scalarity()    :: tbl | var.
-type metric_type()         :: atom().
-type metric_type_param()   :: atom().
-type metric_type_options() :: any().
-type metric_types()        :: [metric_type() | {metric_type(), metric_type_options()}].
-type metrics()             :: [{
                                metric_name(),
                                metric_scalarity(),
                                metric_types()
                               }].
-type row_id()              :: atom() | string() | binary() | integer().
-type metric_id()           :: metric_name() | {metric_name(), row_id()}.

-type select_row_id()    :: all |
                            first |
                            last |
                            {next, row_id()} |
                            {prev, row_id()} |
                            {id, row_id()} |
                            [{id, row_id()}] .
-type select_param(T)    :: all | T | [T].

-type select_query()     :: [
                                {names,  select_param(metric_name())} | 
                                {types,  select_param(metric_type())} | 
                                {params, select_param(metric_type_param())} | 
                                {row_id, select_row_id()}
                            ].

-type select_results_tree(K, V)  :: {K, [V]} | {K, V} | V.
-type select_results_param()     :: select_results_tree(metric_type_param(), metric_value()).
-type select_results_type()      :: select_results_tree(metric_type(),       select_results_param()).
-type select_results_metric()    :: select_results_tree(metric_name(),       select_results_type()).
-type select_results()           :: [select_results_metric()] | select_results_metric().

-type functor()          :: {atom(), atom(), list()} | fun(() -> any()).

-type app_options()      :: [
                                {metrics, metrics()}
                            ].


-export([start/0, stop/0]).
-export([update/2, tc_update/2, tc_update/3]).
-export([get/3, select_all/0, select/1]).
-export([add_metric/3, delete_metric/1]).
-export([measure_call_time/1, measure_call_time/2, measure_call_time_simple/1]).

% @doc Start estatist application.
-spec start() -> ok | {error, term()}.
start() ->
    application:start(?MODULE).

% @doc Stop estatist application.
-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(?MODULE).

% @doc Update specific metric with value, which will be applied to all instances of metric types.
-spec update(metric_id(), metric_input_value()) -> ok | {error, term()}.
update(MetricID, Value) ->
    estatist_core:update(MetricID, Value).

% @doc Same as tc_update(throw, MetricID, Fun)
-spec tc_update(metric_id(), functor()) -> any().
tc_update(MetricID, Fun) ->
    tc_update(throw, MetricID, Fun).

% @doc Run Fun with timer:tc and then update metric with resutls time.
% ThrowFlag means, throw or not exception when update fails.
-spec tc_update(ThrowFlag :: throw | quite, metric_id(), functor()) -> any().
tc_update(throw, MetricID, Fun) ->
    {T, V} = tc(Fun),
    case update(MetricID, T) of
        ok ->
            V;
        Err = {error, _} ->
            throw(Err)
    end;
tc_update(quite, MetricID, Fun) ->
    {T, V} = tc(Fun),
    _ = update(MetricID, T),
    V.

% @doc Get specific value by: metric name, metric type, type param.
-spec get(metric_id(), metric_type(), metric_type_param()) -> metric_value().
get(MetricID, Type, Param) ->
    estatist_core:get(MetricID, Type, Param).

% @doc Same as select([])
-spec select_all() -> {ok, select_results()} | {error, term()}.
select_all() ->
    select([]).

% @doc Select values from all metrics.
% You may specify following conditions:<br/>
% * names -- metrics names;<br/>
% * types -- metrics types;<br/>
% * params -- metrics types params;<br/>
% * row_id -- row identificators, when metric is table.<br/>
% Run without conditions will retutn all information.
% See examples in main README.
% @end
-spec select(select_query()) -> {ok, select_results()} | {error, term()}.
select(QueryPL) ->
    Names  = proplists:get_value(names,  QueryPL, all),
    Types  = proplists:get_value(types,  QueryPL, all),
    Params = proplists:get_value(params, QueryPL, all),
    case proplists:get_value(row_id, QueryPL) of
        undefined ->
            estatist_core:select(Names, Types, Params);
        RowID ->
            estatist_core:select(Names, Types, Params, RowID)
    end.

% @doc Add new metric on runtime.
-spec add_metric(metric_name(), metric_scalarity(), metric_types()) -> ok | {error, term()}.
add_metric(Name, Scalarity, MetricTypes) ->
    estatist_core:add_metric(Name, Scalarity, MetricTypes).

% @doc Delete metric on runtime.
-spec delete_metric(metric_name()) -> ok.
delete_metric(Name) ->
    estatist_core:delete_metric(Name).

% @doc Return only 'mean' result from measure_call_time/1 .
-spec measure_call_time_simple(functor()) -> pos_integer().
measure_call_time_simple(Fun) ->
    proplists:get_value(mean, measure_call_time(Fun)).

% @doc Same as measure_call_time(Fun, {time, 10000000}) .
-spec measure_call_time(functor()) -> basho_metrics_nifs:histogram_stats().
measure_call_time(Fun) ->
    measure_call_time(Fun, {time, 10000000}).

% @doc Measuring function executing time.
% You can set time or count of repeating executions by Options param.
-spec measure_call_time(functor(), Options :: {time | count, pos_integer()}) -> basho_metrics_nifs:histogram_stats().
measure_call_time(Fun, {time, Timeout}) ->
    {T, _} = tc(Fun),
    Count = Timeout div T,
    measure_call_time(Fun, {count, Count});
measure_call_time(Fun, {count, TestCallCount}) ->
    {ok, Histogram} = basho_metrics_nifs:histogram_new([{size, TestCallCount}]),
    ok = measure_call_time_rec(Histogram, Fun, TestCallCount),
    basho_metrics_nifs:histogram_stats(Histogram).


%% @private
measure_call_time_rec(_, _, 0) ->
    ok;
measure_call_time_rec(Histogram, Fun, TestCallCount) ->
    {T, _} = tc(Fun),
    basho_metrics_nifs:histogram_update(Histogram, T),
    measure_call_time_rec(Histogram, Fun, TestCallCount - 1).

%% @private
tc({M, F, A}) ->
    timer:tc(M, F, A);
tc(Fun) ->
    timer:tc(Fun, []).
