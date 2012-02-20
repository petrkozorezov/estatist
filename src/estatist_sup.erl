-module(estatist_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(CHILD(Mod, Args), {Mod, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Config = application:get_all_env(),
    {ok, {{one_for_one, 5, 10},
          [
           ?CHILD(estatist_core, [Config])
          ]} }.

