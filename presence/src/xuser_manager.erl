%%% -------------------------------------------------------------------
%%% Author  : stephbu
%%% Description :
%%%
%%% Created : Dec 7, 2010
%%% -------------------------------------------------------------------
-module(xuser_manager).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Macro Definitions
%% --------------------------------------------------------------------
-define(XUSER_FINDER_ATOM, xuser_finder_singleton).
-define(XUSER_FINDER_PID, global:whereis_name(?XUSER_FINDER_ATOM)).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, get/1, delete/1, notify_friends/2, notify_xusers/2, stop/0]).

%% --------------------------------------------------------------------
%% gen_server Behavioural exports
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {supervisor, addresses}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Args) ->
	case global:whereis_name(?XUSER_FINDER_ATOM) of
		undefined ->
			gen_server:start_link({global, ?XUSER_FINDER_ATOM}, ?MODULE, Args, []);
		Pid -> 
			{ ok, Pid }
	end.

stop() ->
	case global:whereis_name(?XUSER_FINDER_ATOM) of
		undefined -> ok;
		Pid -> 
			case erlang:is_process_alive(Pid) of 
				true ->
					gen_server:call(Pid, stop);
				false ->
					% process is died unregister name
					global:unregister_name(?XUSER_FINDER_ATOM)
			end
	end.
	
% get(binary()) -> pid()
get(Xuid) when erlang:is_binary(Xuid) ->
	gen_server:call({global, ?XUSER_FINDER_ATOM}, {get, Xuid}).

notify_friends(Xuid, Message) when is_binary(Xuid) ->
	gen_server:cast({global, ?XUSER_FINDER_ATOM}, {notify_friends, Xuid, Message}).

notify_xusers(Xuids, Message) when is_list(Xuids) ->
	gen_server:cast({global, ?XUSER_FINDER_ATOM}, {notify_xusers, Xuids, Message}).

delete(Xuid) when erlang:is_binary(Xuid) ->
	gen_server:call({global, ?XUSER_FINDER_ATOM}, {delete, Xuid}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{ addresses=ets:new(?MODULE,[set])}}.
% ets:insert_new(State#state.addresses, {Xuid,Pid}).

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({get, Xuid}, _From, State) when is_binary(Xuid) ->
	UserRow = ets:lookup(State#state.addresses, Xuid),
	case UserRow of
		[] ->
			% We should probably move insert into its own message so it can be call'd and all reads can be casted
			{ok, NewUserPid} = xuser:start(Xuid),
			ets:insert_new(State#state.addresses, { Xuid, NewUserPid }),
			{reply, {created, NewUserPid}, State};

		_ ->
			{ Xuid, Pid } = hd(UserRow),
			{reply, {read, Pid}, State}
	end;

handle_call({delete, Xuid}, _From, State) when is_binary(Xuid) ->
	UserRow = ets:lookup(State#state.addresses, Xuid),
	case UserRow of
		[] ->
			{reply, ok, State};
		_ ->
			{_Xuid, Xuserpid } = hd(UserRow),
			Friends = xuser:get_friends(Xuserpid),
	
			% clear up user server
			ok = xuser:delete(Xuserpid),

			% Spawn process to remove user from friend's friend lists
			spawn(async_friends_cleanup(Xuid, Friends)),
			
			% cleanup ets
			ets:delete(State#state.addresses,Xuid),
			{reply, ok, State}
	end;

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({notify_xusers, Xuids, Message}, State) when is_list(Xuids) ->
	spawn(async_notify_xuids(Xuids, State#state.addresses, Message)),
  	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% TODO: If I'm not a supervisor, you will regret it long run

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% friends_cleanup(binary(), list()) -> undefined
async_friends_cleanup(Xuid, Friends) when is_binary(Xuid), is_list(Friends) ->
	Cleanup = fun(Friendxuid) ->
					  Friendpid = xuser_manager:get(Friendxuid),
					  xuser:remove_friend(Friendpid, Xuid)
			  end,
	lists:foreach(Cleanup, Friends).

async_notify_xuids(Xuids, AddressBook, Message) ->
	Notify = fun(Xuid) ->
		UserRow = ets:lookup(AddressBook, Xuid),
		case UserRow of
		[] ->
			false;			
		_ ->
			{_Xuid, Xuserpid } = hd(UserRow),
			xuser:notify(Xuserpid, Message)
		end
		end,
	lists:foreach(Notify, Xuids).
	

%% --------------------------------------------------------------------
%% Unit Tests
%% --------------------------------------------------------------------

start_test() -> 
	{ok, _Pid} = xuser_finder:start_link([]).

start_stop_test() ->
	{ok, _Pid} = xuser_finder:start_link([]),
	xuser_finder:stop().

insert_and_retrieve_test() ->
	{ok, _Pid} = xuser_finder:start_link([]),
	LookupValue = calendar:universal_time(),
	{Response, UserPid} = xuser_finder:get(LookupValue), 
	?assertEqual(created, Response),
	{ok, Value} = xuser:get_xuid(UserPid),
	?assertEqual(LookupValue, Value),
	{Response2, UserPid2} = xuser_finder:get(LookupValue),
	?assertEqual(read, Response2),
	?assertEqual(UserPid, UserPid2).
