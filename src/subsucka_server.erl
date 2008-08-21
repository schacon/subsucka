-module (subsucka_server).
-export ([start/0, stop/0]).

start() ->
  application:load(subsucka_server),
  application:start(subsucka_server).

stop() ->
  application:stop(subsucka_server).
