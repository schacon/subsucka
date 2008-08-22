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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_revisions(Url) ->
  Info = cmd("svn info " ++ Url, "."),
  case regexp:match(Info, "Revision: [0-9]+") of
    {match, Start, Length} ->
      case regexp:match(R = string:substr(Info, Start, Length), "[0-9]+") of
        {match, Start1, Length1} ->
          {Int, _} = string:to_integer(string:substr(R, Start1, Length1)),
          {ok, Int};
        _ ->
          {error, no_info}
      end;
    _ ->
      {error, no_info}
  end.

import_repo(Url) ->
  Ref  = make_ref(),
  RefL = erlang:ref_to_list(Ref),
  io:format("~p~n", [RefL]),
  io:format("~p~n", [Url]),
  case get_revisions(Url) of
    {ok, Revisions} ->
      go(Url, RefL, Revisions);
    _ ->
      error_logger:error_msg("Unable to get info for ~s", [Url])
  end.

go(Url, Ref, Revisions) ->
  cmd("mkdir '" ++ Ref ++ "'", "."),
  cmd("git init", Ref),
  cmd("echo '.svn' > .gitignore", Ref),
  cmd("git add .; git commit -m \"init with .gitignore\"", Ref),
  checkout(Url, Ref, Revisions).
  
checkout(Url, Ref, Revisions) ->
  checkout(Url, Ref, 1, Revisions).

checkout(Url, Ref, N, Revisions) ->
  Out = cmd("svn co ~s -r ~b .", [Url, N], Ref),
  io:format("~p~n", [Out]),
  case regexp:match(Out, "svn: Unable") of
    {match, _} ->
      io:format("Unable to start at r~b", [N]),
      checkout(Url, Ref, N+1, Revisions);
    _ ->
      update(Ref, N+1, Revisions)
  end.

update(_Ref, N, Revisions) when N > Revisions ->
  ok;
update(Ref, N, Revisions) ->
  io:format("~~ updating ~b of ~b~n", [N, Revisions]),
  cmd("svn update -r~b", [N], Ref),
  cmd("git add .; git commit -m 'r~b'", [N], Ref),
  update(Ref, N+1, Revisions).


cmd(Cmd, Data, Dir) ->
  cmd(io_lib:format(Cmd, Data), Dir).

cmd(Cmd, Dir) ->
  os:cmd("cd '" ++ Dir ++ "'; " ++ Cmd).
