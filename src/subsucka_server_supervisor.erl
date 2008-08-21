-module(subsucka_server_supervisor).
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
    [{slave_manager,
       {slave_manager, start_link, []},
       permanent, 10000, worker, [slave_manager]}
    ]}}.
