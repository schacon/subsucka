-module (subsucka_slave).
-export ([start/0, stop/0]).

start() ->
  application:load(subsucka_slave),
  application:start(subsucka_slave).

stop() ->
  application:stop(subsucka_slave).