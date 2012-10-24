% @author Kozorezov Petr <petr.kozorezov@gmail.com>
% @copyright 2012 Kozorezov Petr
% @private
-module(estatist_module_system_info).
-behavour(estatist_module).

-export([
         init/2,
         get/3,
         update/3,
         tick/1,
         terminate/0
        ]).

-export([
         uptime/0,
         cpu_usage/0,
         process_usage/0,
         io_input/0,
         io_output/0,
         process_count/0
        ]).

init(_, []) ->
    {undefined, undefined}.

get(_, _, all) ->
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
    [{X, ?MODULE:X()} || X <- [cpu_usage, uptime, process_usage, io_input, io_output, process_count]].

uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    UpTime div 1000.

cpu_usage() ->
    {_, WC} = erlang:statistics(wall_clock),
    {_, RT} = erlang:statistics(runtime),
    P = erlang:system_info(logical_processors_online),
    round(100 * RT / (WC + 1) / P).

process_usage() ->
    erlang:system_info(process_count) div erlang:system_info(process_limit) * 100.

io_input() ->
    {{input, Input}, _} = erlang:statistics(io),
    Input.

io_output() ->
    {_, {output, Output}} = erlang:statistics(io),
    Output.

process_count() ->
    erlang:system_info(process_count).