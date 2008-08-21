-module(subversion_import).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, import_uri/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile([export_all]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop).

import_uri(Uri) -> gen_server:call(?MODULE, {import, Uri}).


init([]) -> {ok, []}.

handle_call({import, Uri}, _From, State) ->
  Reply = ok,
  io:format("~p~n", [Uri]),
  {reply, Reply, State};
handle_call(stop, _From, State) ->
  {stop, normal, stopped, []}.
	
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok. 
code_change(_OldVsn, State, _Extra) -> {ok, State}.
