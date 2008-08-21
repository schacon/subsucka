-module(subsucka_slave_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  net_kernel:set_net_ticktime(30),
  subsucka_slave_supervisor:start_link().
  
stop(_State) ->
  ok.