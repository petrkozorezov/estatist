-module(estatist).
-export([start/0, stop/0]).
-export([update/2, update/3, tc_update/2, tc_update/3, get/3, get/4]).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

update(Name, Value) ->
    estatist_core:update(Name, Value).

update(Name, Value, RowID) ->
    estatist_core:update(Name, Value, RowID).

tc_update(Name, Fun) ->
	{T, V} = tc(Fun),
	R = update(Name, T),
	{V, R}.

tc_update(Name, Fun, RowID) ->
	{T, V} = tc(Fun),
	R = update(Name, T, RowID),
	{V, R}.

get(Name, Types, Params) ->
    estatist_core:get(Name, Types, Params).

get(Name, Types, Params, RowID) ->
    estatist_core:get(Name, Types, Params, RowID).

tc({M, F, A}) ->
	timer:tc(M, F, A);
tc(Fun) ->
	timer:tc(Fun, []).
