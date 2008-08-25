-module(subversion_import).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, import_uri/1, import_part/2]).

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
  spawn_link(?MODULE, import_repo, [Uri]),
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
  case get_revisions(Url) of
    {ok, Revisions} ->
      RevList = split_range(Revisions, split_count(Revisions, length(nodes()))),
      Remotes = pmap(fun(A, B) -> import_part(A, B) end, RevList, nodes(), Url),
      io:format("imp: ~p~n", [Remotes]),
      combine_results(Remotes, Url);
    _ ->
      error_logger:error_msg("Unable to get info for ~s", [Url])
  end.
  
import_part(RevTuple, Url) ->
  Ref = new_dir(),
  error_logger:info_msg("import ~p~n", [RevTuple]),
  cmd("git init", Ref),
  cmd("echo '.svn' > .gitignore", Ref),
  cmd("git add .; git commit -m \"init with .gitignore\"", Ref),
  {StartRev, EndRev} = RevTuple,
  checkout_part(Url, Ref, StartRev, EndRev),
  cmd("git gc --aggressive", Ref),
  cmd("git update-server-info", Ref),
  {net_adm:localhost(), Ref}.
  
checkout_part(Url, Ref, N, Revisions) ->
  Out = cmd("svn co ~s -r ~b .", [Url, N], Ref),
  cmd("git add .; git commit -a -m 'r~b'", [N], Ref),
  error_logger:info_msg("~~ updating ~b of ~b~n", [N, Revisions]),
  case regexp:match(Out, "svn: Unable") of
    {match, _} ->
      io:format("Unable to start at r~b", [N]),
      checkout_part(Url, Ref, N+1, Revisions);
    _ ->
      update(Ref, N+1, Revisions)
  end.

update(_Ref, N, Revisions) when N > Revisions ->
  ok;
update(Ref, N, Revisions) ->
  error_logger:info_msg("~~ updating ~b of ~b~n", [N, Revisions]),
  cmd("svn update -r~b", [N], Ref),
  cmd("git add .; git commit -a -m 'r~b'", [N], Ref),
  update(Ref, N+1, Revisions).

combine_results(Remotes, Url) ->
  Ref = new_dir(),
  error_logger:info_msg("combine ~p~n", [Ref]),
  [First|Rest] = Remotes,
  {HostName, Path} = First,
  CloneUrl = "http://" ++ HostName ++ Path ++ "/.git",
  io:format("git clone " ++ CloneUrl ++ " repo"),
  cmd("git clone " ++ CloneUrl ++ " repo", Ref),
  Repo = Ref ++ "/repo",
  combine_rest_results(Rest, Repo, 1),
  rewrite_commits(Repo, Url),
  Repo.
  
rewrite_commits(Repo, Url) ->
  Out = cmd("./rewrite_commits.rb ~p ~p", [Repo, Url], "."),
  io:format("~p~n", [Out]).
  
combine_rest_results([], Repo, Num) -> [];
combine_rest_results(Remotes, Repo, Num) ->
  [Next|Rem] = Remotes,
  {HostName, Path} = Next,
  CloneUrl = "http://" ++ HostName ++ Path ++ "/.git",
  cmd("git remote add r~b ~p", [Num, CloneUrl], Repo),
  cmd("git fetch r~b", [Num], Repo),
  BranchName = {"r" ++ erlang:integer_to_list(Num) ++ "/master"},
  lists:flatten([BranchName, combine_rest_results(Rem, Repo, Num + 1)]).

cmd(Cmd, Data, Dir) ->
  cmd(io_lib:format(Cmd, Data), Dir).

cmd(Cmd, Dir) ->
  os:cmd("cd '" ++ Dir ++ "'; " ++ Cmd).

% no more than 3 per node
% no less than 50 per process  
split_count(RevCount, NodeCount) ->
  InitialGuess = round(RevCount / (NodeCount * 3)),
  case InitialGuess < 25 of
    true  -> round(RevCount / 25);
    false -> (NodeCount * 3)
  end.
  
split_range(Range, Splits) ->
  case Splits > 1 of
    true -> Each = round(Range / Splits),
            list_range([], 1, Each, Range);
    false -> [{1, Range}]
  end.

list_range(Range, Start, Each, Max) ->
  End = Start + Each,
  case End > Max of
    true  -> [{Start, Max}];
    false -> [{Start, End} | list_range(Range, End + 1, Each, Max)]
  end.
  
new_dir() ->  
  {A, B, C} = erlang:now(),           
  random:seed(A, B, C),
  Ref  = "/tmp/import" ++ integer_to_list(random:uniform(100000)),
  cmd("mkdir '" ++ Ref ++ "'", "."),
  Ref.

pmap(Fun, List, Nodes, ExtraArgs) -> 
  SpawnFun =
    case length(Nodes) of
       0 -> fun spawn/1;
       Length ->
         NextNode = fun() -> lists:nth(random:uniform(Length), Nodes) end,
         fun(F) -> spawn(NextNode(), F) end
    end,
  Parent = self(),
  Pids = [SpawnFun(fun() -> Parent ! {self(), (catch Fun(Elem, ExtraArgs))} end)
    || Elem <- List],
  [receive {Pid, Val} -> Val end || Pid <- Pids].