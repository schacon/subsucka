-module(subsucka_supervisor).
-behaviour(supervisor).

-export([start/0, start_link/0, start_shell/0, init/1]).

start() ->
  spawn(fun() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
  end).
  
start_shell() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
  unlink(Pid).  

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).
  
init([]) ->
  {ok, {{one_for_one, 100, 300},
    [{subversion_import,
       {subversion_import, start_link, []},
       permanent, 10000, worker, [subversion_import]},
     {rebar,
       {rebar, start, []},
       permanent, 10000, worker, [rebar]}
    ]}}.
