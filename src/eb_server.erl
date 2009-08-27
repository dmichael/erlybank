%%%-------------------------------------------------------------------
%%% File    : eb_server.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : The ErlyBank account server.
%%%
%%% Created :  5 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(eb_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
        create_account/2,
        deposit/2,
        withdraw/2,
        delete_account/1,
        authorize/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: create_account(Name) -> ok
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
create_account(Name, PIN) ->
  gen_server:cast(?SERVER, {create, Name, PIN}).

%%--------------------------------------------------------------------
%% Function: deposit(Name, Amount) -> {ok, Balance} | {error, Reason}
%% Description: Deposits Amount into Name's account. Returns the
%% balance if successful, otherwise returns an error and reason.
%%--------------------------------------------------------------------
deposit(Name, Amount) ->
  gen_server:call(?SERVER, {deposit, Name, Amount}).

%%--------------------------------------------------------------------
%% Function: withdraw(Name, Amount) -> {ok, Balance} | {error, Reason}
%% Description: Withdraws Amount from Name's account.
%%--------------------------------------------------------------------
withdraw(Name, Amount) ->
  gen_server:call(?SERVER, {withdraw, Name, Amount}).

%%--------------------------------------------------------------------
%% Function: delete_account(Name) -> ok
%% Description: Deletes the account with the name Name.
%%--------------------------------------------------------------------
delete_account(Name) ->
  gen_server:cast(?SERVER, {destroy, Name}).

%%--------------------------------------------------------------------
%% Function: authorize(Name, Pin) -> ok | {error, Reason}
%% Description: Authorizes the account Name with PIN
%%--------------------------------------------------------------------
authorize(Name, PIN) ->
  gen_server:call(?SERVER, {authorize, Name, PIN}).

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
  eb_event_manager:add_handler(eb_withdrawal_handler),
  {ok, dict:new()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({authorize, Name, PIN}, _From, State) ->
  case dict:find(Name, State) of
    {ok, {PIN, _Value}} ->
      {reply, ok, State};
    {ok, {_OtherPIN, _Value}} ->
      {reply, {error, invalid_pin}, State};
    error ->
      {reply, {error, account_does_not_exist}, State}
  end;
handle_call({deposit, Name, Amount}, _From, State) ->
  case dict:find(Name, State) of
    {ok, {PIN, Value}} ->
      NewBalance = Value + Amount,
      Response = {ok, NewBalance},
      NewState = dict:store(Name, {PIN, NewBalance}, State),
      {reply, Response, NewState};
    error ->
      {reply, {error, account_does_not_exist}, State}
  end;
handle_call({withdraw, Name, Amount}, _From, State) ->
  case dict:find(Name, State) of
    {ok, {_PIN, Value}} when Value < Amount ->
      {reply, {error, not_enough_funds}, State};
    {ok, {PIN, Value}} ->
      NewBalance = Value - Amount,
      NewState = dict:store(Name, {PIN, NewBalance}, State),
      % Send notification
      eb_event_manager:notify({withdraw, Name, Amount, NewBalance}),
      {reply, {ok, NewBalance}, NewState};
    error ->
      {reply, {error, account_does_not_exist}, State}
  end;
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({create, Name, PIN}, State) ->
  {noreply, dict:store(Name, {PIN, 0}, State)};
handle_cast({destroy, Name}, State) ->
  {noreply, dict:erase(Name, State)};
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
