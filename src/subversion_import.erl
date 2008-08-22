-module(subversion_import).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, import_uri/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile([export_all]).

start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
stop()       -> gen_server:call(?MODULE, stop).

import_uri(Uri) -> gen_server:call({global, ?MODULE}, {import, Uri}).


init([]) -> 
  error_logger:info_msg("Starting up SVN server.~n"),
  net_kernel:monitor_nodes(true, [nodedown_reason]),
  {ok, []}.

handle_call({import, Uri}, _From, State) ->
  Reply = ok,
  io:format("~p~n", [Uri]),
  %%io:format("Nodes: ~p~n", nodes()),
  {reply, Reply, State};
handle_call(stop, _From, State) ->
  {stop, normal, stopped, []}.
	
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({nodeup, Node, _}, State) ->
  error_logger:info_msg("Node ~p joined.~n", [Node]),
  {noreply, State};
handle_info({nodedown, Node, Reason}, State) ->
  error_logger:warning_msg("Node ~p went away because ~p~n", [Node, Reason]),
  {noreply, State}.
  
terminate(_Reason, _State) -> ok. 
code_change(_OldVsn, State, _Extra) -> {ok, State}.
