-module(estatist_module).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 2}, {tick, 1}, {get, 3}, {update, 3}];
behaviour_info(_) ->
    undefined.
