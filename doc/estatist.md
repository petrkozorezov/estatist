

#Module estatist#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Fast application statistics agregator for erlang, based on basho_metrics.



Copyright (c) 2012 Kozorezov Petr

__Authors:__ Kozorezov Petr ([`petr.kozorezov@gmail.com`](mailto:petr.kozorezov@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-app_options">app_options()</a>##



<pre>app_options() = [{metrics, <a href="#type-metrics">metrics()</a>}]</pre>



###<a name="type-functor">functor()</a>##



<pre>functor() = {atom(), atom(), list()} | fun(() -&gt; any())</pre>



###<a name="type-metric_id">metric_id()</a>##



<pre>metric_id() = <a href="#type-metric_name">metric_name()</a> | {<a href="#type-metric_name">metric_name()</a>, <a href="#type-row_id">row_id()</a>}</pre>



###<a name="type-metric_input_value">metric_input_value()</a>##



<pre>metric_input_value() = number()</pre>



###<a name="type-metric_name">metric_name()</a>##



<pre>metric_name() = atom() | string()</pre>



###<a name="type-metric_scalarity">metric_scalarity()</a>##



<pre>metric_scalarity() = tbl | var</pre>



###<a name="type-metric_type">metric_type()</a>##



<pre>metric_type() = atom()</pre>



###<a name="type-metric_type_options">metric_type_options()</a>##



<pre>metric_type_options() = any()</pre>



###<a name="type-metric_type_param">metric_type_param()</a>##



<pre>metric_type_param() = atom()</pre>



###<a name="type-metric_types">metric_types()</a>##



<pre>metric_types() = [<a href="#type-metric_type">metric_type()</a> | {<a href="#type-metric_type">metric_type()</a>, <a href="#type-metric_type_options">metric_type_options()</a>}]</pre>



###<a name="type-metric_value">metric_value()</a>##



<pre>metric_value() = <a href="#type-metric_input_value">metric_input_value()</a> | undefined</pre>



###<a name="type-metrics">metrics()</a>##



<pre>metrics() = [{<a href="#type-metric_name">metric_name()</a>, <a href="#type-metric_scalarity">metric_scalarity()</a>, <a href="#type-metric_types">metric_types()</a>}]</pre>



###<a name="type-row_id">row_id()</a>##



<pre>row_id() = atom() | string() | binary() | integer()</pre>



###<a name="type-select_param">select_param()</a>##



<pre>select_param(T) = all | T | [T]</pre>



###<a name="type-select_query">select_query()</a>##



<pre>select_query() = [{names, <a href="#type-select_param">select_param</a>(<a href="#type-metric_name">metric_name()</a>)} | {types, <a href="#type-select_param">select_param</a>(<a href="#type-metric_type">metric_type()</a>)} | {params, <a href="#type-select_param">select_param</a>(<a href="#type-metric_type_param">metric_type_param()</a>)} | {row_id, <a href="#type-select_row_id">select_row_id()</a>}]</pre>



###<a name="type-select_results">select_results()</a>##



<pre>select_results() = [<a href="#type-select_results_metric">select_results_metric()</a>] | <a href="#type-select_results_metric">select_results_metric()</a></pre>



###<a name="type-select_results_metric">select_results_metric()</a>##



<pre>select_results_metric() = <a href="#type-select_results_tree">select_results_tree</a>(<a href="#type-metric_name">metric_name()</a>, <a href="#type-select_results_type">select_results_type()</a>)</pre>



###<a name="type-select_results_param">select_results_param()</a>##



<pre>select_results_param() = <a href="#type-select_results_tree">select_results_tree</a>(<a href="#type-metric_type_param">metric_type_param()</a>, <a href="#type-metric_value">metric_value()</a>)</pre>



###<a name="type-select_results_tree">select_results_tree()</a>##



<pre>select_results_tree(K, V) = {K, [V]} | {K, V} | V</pre>



###<a name="type-select_results_type">select_results_type()</a>##



<pre>select_results_type() = <a href="#type-select_results_tree">select_results_tree</a>(<a href="#type-metric_type">metric_type()</a>, <a href="#type-select_results_param">select_results_param()</a>)</pre>



###<a name="type-select_row_id">select_row_id()</a>##



<pre>select_row_id() = all | first | last | {next, <a href="#type-row_id">row_id()</a>} | {prev, <a href="#type-row_id">row_id()</a>} | {id, <a href="#type-row_id">row_id()</a>} | [{id, <a href="#type-row_id">row_id()</a>}]</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_metric-3">add_metric/3</a></td><td>Add new metric on runtime.</td></tr><tr><td valign="top"><a href="#delete_metric-1">delete_metric/1</a></td><td>Delete metric on runtime.</td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td>Get specific value by: metric name, metric type, type param.</td></tr><tr><td valign="top"><a href="#measure_call_time-1">measure_call_time/1</a></td><td>Same as measure_call_time(Fun, {time, 10000000}) .</td></tr><tr><td valign="top"><a href="#measure_call_time-2">measure_call_time/2</a></td><td>Measuring function executing time.</td></tr><tr><td valign="top"><a href="#measure_call_time_simple-1">measure_call_time_simple/1</a></td><td>Return only 'mean' result from measure_call_time/1 .</td></tr><tr><td valign="top"><a href="#select-1">select/1</a></td><td>Select values from all metrics.</td></tr><tr><td valign="top"><a href="#select_all-0">select_all/0</a></td><td>Same as select([]).</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start estatist application.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stop estatist application.</td></tr><tr><td valign="top"><a href="#tc_update-2">tc_update/2</a></td><td>Same as tc_update(throw, MetricID, Fun).</td></tr><tr><td valign="top"><a href="#tc_update-3">tc_update/3</a></td><td>Run Fun with timer:tc and then update metric with resutls time.</td></tr><tr><td valign="top"><a href="#update-2">update/2</a></td><td>Update specific metric with value, which will be applied to all instances of metric types.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add_metric-3"></a>

###add_metric/3##




<pre>add_metric(Name::<a href="#type-metric_name">metric_name()</a>, Scalarity::<a href="#type-metric_scalarity">metric_scalarity()</a>, MetricTypes::<a href="#type-metric_types">metric_types()</a>) -> ok | {error, term()}</pre>
<br></br>




Add new metric on runtime.<a name="delete_metric-1"></a>

###delete_metric/1##




<pre>delete_metric(Name::<a href="#type-metric_name">metric_name()</a>) -> ok</pre>
<br></br>




Delete metric on runtime.<a name="get-3"></a>

###get/3##




<pre>get(MetricID::<a href="#type-metric_id">metric_id()</a>, Type::<a href="#type-metric_type">metric_type()</a>, Param::<a href="#type-metric_type_param">metric_type_param()</a>) -> <a href="#type-metric_value">metric_value()</a></pre>
<br></br>




Get specific value by: metric name, metric type, type param.<a name="measure_call_time-1"></a>

###measure_call_time/1##




<pre>measure_call_time(Fun::<a href="#type-functor">functor()</a>) -> <a href="basho_metrics_nifs.md#type-histogram_stats">basho_metrics_nifs:histogram_stats()</a></pre>
<br></br>




Same as measure_call_time(Fun, {time, 10000000}) .<a name="measure_call_time-2"></a>

###measure_call_time/2##




<pre>measure_call_time(Fun::<a href="#type-functor">functor()</a>, Options::{time | count, pos_integer()}) -> <a href="basho_metrics_nifs.md#type-histogram_stats">basho_metrics_nifs:histogram_stats()</a></pre>
<br></br>




Measuring function executing time.
You can set time or count of repeating executions by Options param.<a name="measure_call_time_simple-1"></a>

###measure_call_time_simple/1##




<pre>measure_call_time_simple(Fun::<a href="#type-functor">functor()</a>) -> pos_integer()</pre>
<br></br>




Return only 'mean' result from measure_call_time/1 .<a name="select-1"></a>

###select/1##




<pre>select(QueryPL::<a href="#type-select_query">select_query()</a>) -> {ok, <a href="#type-select_results">select_results()</a>} | {error, term()}</pre>
<br></br>




Select values from all metrics.
You may specify following conditions:
<br></br>

* names -- metrics names;
<br></br>

* types -- metrics types;
<br></br>

* params -- metrics types params;
<br></br>

* row_id -- row identificators, when metric is table.
<br></br>

Run without conditions will retutn all information.
See examples in main README.<a name="select_all-0"></a>

###select_all/0##




<pre>select_all() -> {ok, <a href="#type-select_results">select_results()</a>} | {error, term()}</pre>
<br></br>




Same as select([])<a name="start-0"></a>

###start/0##




<pre>start() -&gt; ok | {error, term()}</pre>
<br></br>




Start estatist application.<a name="stop-0"></a>

###stop/0##




<pre>stop() -&gt; ok | {error, term()}</pre>
<br></br>




Stop estatist application.<a name="tc_update-2"></a>

###tc_update/2##




<pre>tc_update(MetricID::<a href="#type-metric_id">metric_id()</a>, Fun::<a href="#type-functor">functor()</a>) -> any()</pre>
<br></br>




Same as tc_update(throw, MetricID, Fun)<a name="tc_update-3"></a>

###tc_update/3##




<pre>tc_update(ThrowFlag::throw | quite, MetricID::<a href="#type-metric_id">metric_id()</a>, Fun::<a href="#type-functor">functor()</a>) -> any()</pre>
<br></br>




Run Fun with timer:tc and then update metric with resutls time.
ThrowFlag means, throw or not exception when update fails.<a name="update-2"></a>

###update/2##




<pre>update(MetricID::<a href="#type-metric_id">metric_id()</a>, Value::<a href="#type-metric_input_value">metric_input_value()</a>) -> ok | {error, term()}</pre>
<br></br>




Update specific metric with value, which will be applied to all instances of metric types.