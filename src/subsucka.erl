-module (subsucka).
-export ([start/0, stop/0]).

start() ->
  application:load(subsucka),
  application:start(subsucka).

stop() ->
  application:stop(subsucka).
