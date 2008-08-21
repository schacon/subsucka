-module(subsucka_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  subsucka_server:start_link().
  
stop(_State) ->
  subsucka_server:stop().