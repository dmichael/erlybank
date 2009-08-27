%%%-------------------------------------------------------------------
%%% File    : eb_atm.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : The ATM backend for ErlyBank
%%%
%%% Created :  6 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(eb_atm).

-behaviour(gen_fsm).

%% API
-export([start_link/0,
        authorize/2]).

%% gen_fsm callbacks
-export([init/1, unauthorized/2, unauthorized/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
         authorized/2, authorized/3, deposit/1, thank_you/2, thank_you/3,
         withdraw/1, cancel/0]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: authorize(Name, Pin) -> ok | {error, Reason}
%%--------------------------------------------------------------------
authorize(Name, PIN) ->
  gen_fsm:sync_send_event(?SERVER, {authorize, Name, PIN}).

%%--------------------------------------------------------------------
%% Function: deposit(Amount) -> ok
%% Description: Deposits a certain amount in the currently authorized
%% account.
%%--------------------------------------------------------------------
deposit(Amount) ->
  gen_fsm:send_event(?SERVER, {deposit, Amount}).

%%--------------------------------------------------------------------
%% Function: withdraw(Amount) -> {ok, Balance} | {error, Reason}
%% Description: Withdraws money from the currently authorized account
%%--------------------------------------------------------------------
withdraw(Amount) ->
  gen_fsm:sync_send_event(?SERVER, {withdraw, Amount}).

%%--------------------------------------------------------------------
%% Function: cancel/0
%% Description: Cancels the ATM transaction no matter what state.
%%--------------------------------------------------------------------
cancel() ->
  gen_fsm:send_all_state_event(?SERVER, cancel).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([]) ->
  {ok, unauthorized, nobody}.

%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------
unauthorized(_Event, State) ->
  {next_state, unauthorized, State}.

authorized({deposit, Amount}, State) ->
  eb_server:deposit(State, Amount),
  {next_state, thank_you, State, 5000};
authorized(_Event, State) ->
  {next_state, authorized, State}.

thank_you(timeout, _State) ->
  {next_state, unauthorized, nobody};
thank_you(_Event, _State) ->
  {next_state, unauthorized, nobody}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
unauthorized({authorize, Name, Pin}, _From, State) ->
  case eb_server:authorize(Name, Pin) of
    ok ->
      {reply, ok, authorized, Name};
    {error, Reason} ->
      {reply, {error, Reason}, unauthorized, State}
  end;
unauthorized(_Event, _From, State) ->
  Reply = {error, invalid_message},
  {reply, Reply, unauthorized, State}.

authorized({withdraw, Amount}, _From, State) ->
  case eb_server:withdraw(State, Amount) of
    {ok, Balance} ->
      {reply, {ok, Balance}, thank_you, State, 5000};
    {error, Reason} ->
      {reply, {error, Reason}, authorized, State}
  end;
authorized(_Msg, _From, State) ->
  {reply, {error, invalid_message}, authorized, State}.

thank_you(_Msg, _From, State) ->
  {reply, {error, invalid_message}, unauthorized, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(cancel, _StateName, _State) ->
  {next_state, unauthorized, nobody};
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = {error, invalid_message},
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
