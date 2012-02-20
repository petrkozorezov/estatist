-module(estatist).
-export([start/0, stop/0]).
-export([update/2, update/3, get/3, get/4]).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

update(Name, Value) ->
    estatist_core:update(Name, Value).

update(Name, Value, RowID) ->
    estatist_core:update(Name, Value, RowID).

get(Name, Types, Params) ->
    estatist_core:get(Name, Types, Params).

get(Name, Types, Params, RowID) ->
    estatist_core:get(Name, Types, Params, RowID).

%% TODO
%%get_next() ->
%%    ok.
