-module(subsucka_slave_supervisor).
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
  Master = application:get_env(master),
  case Master of
    {ok, MasterNode} ->
      ping_master(MasterNode);
    undefined ->
      MasterNode = node()
  end,
  
  {ok, {{one_for_one, 100, 300},
    [{subversion_import,
       {subversion_import, start_link, []},
        permanent, 10000, worker, [subversion_import]},
     {master_beater,
      {master_beater, start_link, [MasterNode, 10000, 5000]},
       permanent, 10000, worker, [master_beater]}
    ]}}.
    
% Helper functions

ping_master(Node) -> 
  case net_adm:ping(Node) of
    pong -> 
      error_logger:info_msg("Master node available.~n", [Node]),
      timer:sleep(10000),
      ok;
    pang -> 
      error_logger:info_msg("Master node ~p not available. Retrying in 5 seconds.~n", [Node]),
      timer:sleep(10000),
      ping_master(Node)
  end.