-module(estatist_core).
-behaviour(gen_server).

-define(SERVER, {global, ?MODULE}).

%% TODO terminate

%%
%% API
%%
-export([
         start_link/1,
         stop/1,
         update/2,
         update/3,
         get/3,
         get/4,
         test/0
        ]).

%%
%% gen_server callbacks
%%
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%%
%% API
%%
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

stop(Reason) ->
    gen_server:call(?MODULE, {stop, Reason}).

update(Name, Value) ->
    try
        {Name, var, Contexts} = get_metric(Name),
        update_by_contexts(Name, Contexts, Value),
        ok
    catch
        _:E -> {error, E}
    end.


update(Name, Value, InputRowID) ->
    try
        RowID = correct_row_id(InputRowID),
        {Name, tbl, {Tid, MagicTuples}} = get_metric(Name),
        {RowName, Contexts} = get_insert_tbl_row(Tid, Name, RowID, MagicTuples),
        update_by_contexts({Name, RowName}, Contexts, Value),
        ok
    catch
        _:E -> {error, E}
    end.

%% MetricNames = metric | all_metrics | [metric1, metric2]
%% Params = param | all_params | [param1, param2]
%% для one param | all params | param and param2 соответственно
%% в результате будет param_value | [{param_name, param_value}] | [param1_value, param2_value]
%% аналогично для Types (только all_params -> all_types)

get(Names, Types, Params) ->
    F = fun({Name, var, Contexts}) ->
                get_from_contexts(Name, Types, Params, Contexts);
           ({Name, tbl, {_, _}}) ->
                get(Name, Types, Params, all)
        end,
    get(F, Names).

%% RowSecificID = {id, RowID} | first | last | {next, RowID} | {prev, RowID} | all | [{id, RowID}]
%% TODO refactoring
get(Names, Types, Params, RowID) ->
    F = fun({Name, tbl, {Tid, _}}) ->
                GetFromRow = fun(ID) ->
                                     case get_tbl_row(Tid, ID) of
                                         undefined ->
                                             undefined;
                                         {RowName, Contexts} ->
                                             get_from_contexts({Name, RowName}, Types, Params, Contexts)
                                     end
                             end,
                case RowID of
                    {id, ID} ->
                        GetFromRow(correct_row_id(ID));
                    first ->
                        GetFromRow(ets:first(Tid));
                    last ->
                        GetFromRow(ets:last(Tid));
                    {next, ID} ->
                        GetFromRow(ets:next(Tid, correct_row_id(ID)));
                    {prev, ID} ->
                        GetFromRow(ets:prev(Tid, correct_row_id(ID)));
                    all ->
                        F1 = fun({RowName, Contexts}, Acc) ->
                                    [{RowName, get_from_contexts({Name, RowName}, Types, Params, Contexts)} | Acc]
                            end,
                        lists:reverse(ets:foldl(F1, [], Tid));
                    List when is_list(List) ->
                        lists:map(fun({id, ID}) -> GetFromRow(correct_row_id(ID)) end, List)
                end;
           ({_, var, {_, _}}) ->
                undefined
        end,
    get(F, Names).

%%
%% gen_server callbacks
%%

%% ets таблица в ней все метрики и их контексты
%% {name, var, [{metric_type, mod, context}]}
%% {name, tbl, {Tid, [{metric_type, mod, [options]}]}}
%% для табличных метрик создаются отдельные таблицы, в них
%% {row_name, [{metric_type, mod, context}]}

init(Options) ->

    ets:new(?MODULE, [named_table, set, public]),

    Metrics = proplists:get_value(metrics, Options, []),
    Modules = proplists:get_value(modules, Options, []),

    InitMetric =
        fun({Name, Scalarity, MetricTypes}) when is_atom(Name) and ((Scalarity == var) or (Scalarity == tbl)) ->
                Context = init_metric(Scalarity, Name, MetricTypes, Modules),
                true = ets:insert_new(?MODULE, {Name, Scalarity, Context});
           (IncorrectMetric) ->
                throw({incorrect_metric, IncorrectMetric})
        end,

    lists:foreach(InitMetric, Metrics),

    {ok, undefined}.

handle_call({stop, Reason}, _, State) ->
    {stop, Reason, ok, State};

handle_call({add_tbl_row, Tid, Name, RowName, MagicTuples}, _, State) ->
    %% todo lookup
    Reply = case get_tbl_row(Tid, RowName) of
        undefined ->
            InitMetricType =
                fun(MagicTuple) ->
                        init_metric_type({Name, RowName}, MagicTuple)
                end,
            Value = {RowName, lists:map(InitMetricType, MagicTuples)},
            true = ets:insert_new(Tid, Value),
            {ok, Value};
        _ ->
            {error, dublicate}
    end,
    {reply, Reply, State};

handle_call(_, _, State) ->
    {noreply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({tick, {Mod, Context}}, State) ->
    Mod:tick(Context),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%
%% Local functions
%%
init_metric(var, Name, MetricTypes, Modules) ->
    InitMetricType =
        fun(MetricType) ->
                init_metric_type(Name, make_magic_tuple(MetricType, Modules))
        end,
    lists:map(InitMetricType, MetricTypes);

init_metric(tbl, _Name, MetricTypes, Modules) ->
    F = fun(MetricType) ->
                make_magic_tuple(MetricType, Modules)
        end,
    %%io:format(" init table \"~p\" ~p~n", [Name, MetricTypes]),
    {ets:new(?MODULE, [public, set]), lists:map(F, MetricTypes)}.

make_magic_tuple(MetricType, Modules) ->
    {SplittedMetricType, Options} = split_metric_type_option(MetricType),
    Mod = get_metric_type_module(SplittedMetricType, Modules),
    {SplittedMetricType, Mod, Options}.

init_metric_type(Name, {MetricType, Mod, Options}) ->
    {Context, Tick} = Mod:init(Name, Options),
    schedule_tick(Tick, Context, Mod),
    %%io:format(" init \"~p\" [~p]: ~p~n", [Name, MetricType, Context]),
    {MetricType, Mod, Context}.

get_metric_type_module(MetricType, Modules) ->
    case proplists:get_value(MetricType, Modules) of
        undefined ->
            list_to_atom("estatist_module_" ++ atom_to_list(MetricType));
        M ->
            M
    end.

split_metric_type_option(MetricType) ->
    case MetricType of
        {MT, Opt} ->
            {MT, Opt};
        MT ->
            {MT, []}
    end.    

get(F, all_metrics) ->
    ets:foldr(fun(Metric={Name, _ ,_}, Acc) -> [{Name, F(Metric)} | Acc] end, [], ?MODULE);
get(F, Names) when is_list(Names) ->
    lists:map(fun(Name) -> {Name, F(get_metric(Name))} end, Names);
get(F, Name) when is_atom(Name) ->
    F(get_metric(Name)).

get_metric(Name) when is_atom(Name)->
    case ets:lookup(?MODULE, Name) of
        [] ->
            erlang:throw({unknown_metric, Name});
        [E] ->
            E
    end.

get_from_contexts(Name, all_types, Params, Contexts) ->
    F = fun({Type, Mod, Context}) ->
                {Type, Mod:get(Name, Context, Params)}
        end,
    lists:map(F, Contexts);
get_from_contexts(Name, Types, Params, Contexts) when is_list(Types) ->
    lists:zip(Types, lists:map(fun(Type) -> get_from_contexts(Name, Type, Params, Contexts) end, Types));
get_from_contexts(Name, Type, Params, Contexts) when is_atom(Type) ->
    case lists:keyfind(Type, 1, Contexts) of
        false ->
            erlang:throw({type_for_this_metric_not_found, Name, Type});
        {Type, Mod, Context} ->
            Mod:get(Name, Context, Params)
    end.

update_by_contexts(Name, Contexts, Value) ->
    F = fun({_Type, Mod, Context}) ->
                Mod:update(Name, Context, Value)
        end,
    lists:foreach(F, Contexts),
    ok.

get_insert_tbl_row(Tid, Name, RowID, MagicTuples) ->
    case get_tbl_row(Tid, RowID) of
        undefined ->
            case add_tbl_row(Tid, Name, RowID, MagicTuples) of
                {ok, V} ->
                    V;
                {error, dublicate} ->
                    get_insert_tbl_row(Tid, Name, RowID, MagicTuples)
            end;
        V ->
            V
    end.

add_tbl_row(Tid, Name, RowName, MagicTuples) ->
    gen_server:call(?MODULE, {add_tbl_row, Tid, Name, RowName, MagicTuples}).

get_tbl_row(Tid, RowID) ->
    case ets:lookup(Tid, RowID) of
        [] ->
            undefined;
        [E] ->
            E
    end.


correct_row_id(Int) when is_integer(Int) ->
    integer_to_list(Int);
correct_row_id(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
correct_row_id(List) when is_list(List) ->
    lists:map(fun(E) when is_integer(E) -> E;
                 (_) ->
                      throw({incorrect_row_id, List})
              end, List);
correct_row_id(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
correct_row_id(E) ->
    throw({incorrect_row_id, E}).


schedule_tick(undefined, _, _) ->
    ok;
schedule_tick(Tick, Context, Mod) ->
    {ok, _} = timer:send_interval(Tick, {tick, {Mod, Context}}).

%% TODO auto test
test() ->
    Options = [
               {metrics, [
                          %%{online_counte1, var, [counter]},
                          {online_counter, var, [counter]},
                          {connects,       var, [meter]},
                          {game_requests,  tbl, [histogram, meter]},
                          {player_load,    var, [{meter, [{tick, 1000}]}, {histogram, [{size, 1000}]}]},
                          {player_save,    var, [meter, histogram]}
                         ]},
               {modules, [
                          {meter, estatist_module_meter}
                         ]}
              ],
    {ok, _Pid} = start_link(Options),
    update(online_counter, 1),
    update(player_save, 1),
    update(player_save, 100),
    F = fun(V) ->
                timer:sleep(1000),
                io:format("get \"online_counter\": ~640p ~n", [get(online_counter, counter, count)]),
                io:format("get \"player_save\" meter: ~640p ~n", [get(player_save, meter, [one, five, fifteen])]),
                io:format("get \"player_save\" histogram: ~640p ~n", [get(player_save, histogram, [min, max, mean, count, stddev, p50, p95, p99])]),

                update(game_requests, 100, list_to_atom(integer_to_list(V))),
                io:format("get \"game_requests\" all: ~640p ~n", [get(game_requests, all_types, all_params)]),
                io:format("all: ~640p ~n", [get(all_metrics, all_types, all_params)])
                
        end,
    lists:foreach(F, [2,1,3,0]),
    stop(normal),
    ok.

