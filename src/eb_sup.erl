%%%-------------------------------------------------------------------
%%% File    : eb_sup.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : 
%%%
%%% Created :  7 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(eb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
  EventManager = {eb_event_manager,{eb_event_manager, start_link,[]},
            permanent,2000,worker,dynamic},
  Server = {eb_server, {eb_server, start_link, []},
            permanent,2000,worker,[eb_server]},
  ATM = {eb_atm, {eb_atm, start_link, []},
         permanent,2000,worker,[eb_atm]},
  {ok,{{one_for_one,5,10}, [EventManager, Server, ATM]}}.

%%====================================================================
%% Internal functions
%%====================================================================
