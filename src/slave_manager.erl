-module(slave_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, watch/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile([export_all]).

-record(state, 
  {
    node_pids = dict:new()
  }
).

start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop).

watch(Pid) -> 
  gen_server:cast(?MODULE, {watch, Pid}).
  
identity() ->
  gen_server:call({global, ?MODULE}, {identity}).
  
  
init([]) -> 
  io:format("~p starting~n", [?MODULE]),
  net_kernel:monitor_nodes(true, [nodedown_reason]),
  {ok, []}.

handle_call({identity}, _From, State) ->
  {reply, {ok, node()}, State};
handle_call(stop, _From, State) ->
  {stop, normal, stopped, []}.
  
handle_cast({watch, Pid}, State) ->
  Node = node(Pid),
  error_logger:warning_msg("Now monitoring node ~p~n", [Node]),
  error_logger:info_msg("Now monitoring node ~p~n", [Node]),
  #state{node_pids=NodePids} = State,

  case dict:find(Node, NodePids) of
    {ok, _Pids} ->
      NodePidsNew = dict:append(Node, Pid, NodePids);
    error ->
      NodePidsNew = dict:store(Node, [Pid], NodePids)
  end,

  {noreply, State#state{node_pids=NodePidsNew}}.
  
  
handle_info({nodeup, Node, _}, State) ->
  error_logger:info_msg("Node ~p joined.~n", [Node]),
  {noreply, State};
handle_info({nodedown, Node, Reason}, State) ->
  error_logger:warning_msg("Node ~p went away because ~p. Removing from pools.~n", [Node, Reason]),
  #state{node_pids=NodePids} = State,
  case dict:find(Node, NodePids) of
    error -> 
      {noreply, State};
    {ok, Pids} -> 
      NodePidsNew = dict:erase(Node, NodePids),
      {noreply, State#state{node_pids=NodePidsNew}}
  end.	

terminate(_Reason, _State) -> ok. 
code_change(_OldVsn, State, _Extra) -> {ok, State}.
