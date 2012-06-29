

#The estatist application#


Copyright (c) 2012 Petr Kozorezov


__Authors:__ Petr Kozorezov ([`petr.kozorezov@gmail.com`](mailto:petr.kozorezov@gmail.com)).


###<a name="Overview">Overview</a>##





Estatist is the fast statistics aggregator for erlang applications,
based on basho_metrics and est tables. This all allows to reduce perfomance costs,
and metric update time to around 1-2 microseconds.
As consequence of using ets tables, application can aggregate only from local node.



Workflow is:

 

* set metrics in config;
 * run application;
 * update metrics with `estatist:update` or `estatist:tc_update`;
 * get value by http interface or by `estatist:select`.



Also see http interface library [estatist_http](https://github.com/petrkozorezov/estatist_http).




###<a name="Make">Make</a>##




You can build with `make`, run dialyzer with `make dialyzer`, generate docs with `make doc`.




###<a name="Example">Example</a>##





####<a name="Simple">Simple</a>##



<pre>estatist:start().
estatist:add_metric(online_counter, var, [counter]).
estatist:update(online_counter, 1).
estatist:select_all().</pre>



####<a name="Complex">Complex</a>##


<pre>application:set_env(estatist, [{metrics, [{online_counter, var, [counter]}, {tests, tbl, [simple_meter, histogram]}, {system, var, [system_info]}]}]).
estatist:start().

estatist:update(online_counter, 1).
estatist:tc_update({rps, "lists:seq"}, {lists, seq, [1, 1000]}).
estatist:tc_update({rps, "lists:seq"}, {lists, seq, [1, 1000]}).

estatist:select([{names, online_counter}, {types, counter}, {params, count}]).
estatist:select([{names, online_counter}, {types, simple_meter}]).
estatist:select([{names, [online_counter, tests]}]).</pre>




####<a name="Configuration_example">Configuration example</a>##





Configuration complex example from real game server:

<pre>{estatist, [
  {metrics, [
    %% network

    % online users counter
    {connections,        var, [counter]},

    % network requests per second and timings                 
    {net_commands,       tbl, [histogram, simple_meter]},

    % connects per second
    {connects,           var, [simple_meter]},

    % disconnects per second
    {disconnects,        var, [simple_meter]},

    % disconnects reasons
    {disconnect_reasons, tbl, [{simple_meter, [{tick, 60000}]}]}, 


    %% DB

    % player saves per second and timings
    {player_save,  var, [simple_meter, histogram]},

    % player loads per second and timingstime
    {player_load,  var, [simple_meter, histogram]},

    %% system

    % system info: CPU, memory and etc
    {system,         var, [memory, system_info]},

    % log errors from per modules per minutes
    {modules_errors, tbl, [{simple_meter, [{tick, 60000}]}]}
  ]}
]}</pre>



####<a name="License">License</a>##


<pre>Copyright (c) 2012 Petr Kozorezov
Authors: Petr Kozorezov petr.kozorezov@gmail.com

The contents of this file are subject to the Erlang Public License,
Version 1.1, (the "License"); you may not use this file except in
compliance with the License. You should have received a copy of the
Erlang Public License along with this software. If not, it can be
retrieved online at http://www.erlang.org/.

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
the License for the specific language governing rights and limitations
under the License.</pre>

##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="estatist.md" class="module">estatist</a></td></tr></table>

