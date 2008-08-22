-module(svn_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-compile([export_all]).

-define (IDLE_TIMEOUT, 20000).
-define (MAX_IMPORTS, 200).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  io:format("~~ starting subversion server~n", []),
  {ok, []}.
  
import(Url) ->
  gen_server:call(?MODULE, {import, Url}, ?IDLE_TIMEOUT).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({import, Url}, _From, State) ->
  Reply = ok,
  spawn_link(?MODULE, import_repo, [Url]),
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  io:format("~~ stopping subversion server~n", []),
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

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
