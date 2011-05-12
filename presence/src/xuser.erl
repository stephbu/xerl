%%% -------------------------------------------------------------------
%%% Author  : stephbu
%%% Description :
%%%
%%% Created : Dec 7, 2010
%%% -------------------------------------------------------------------

-module(xuser).
-author("stephbu@microsoft.com").
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

% inactivity terminates process after 30 seconds
-define( PROXY_TIMEOUT, (30*1000) ).
-define( INACTIVITY_TIMEOUT, (5*60*1000) ).
-define( XUSERFRIENDTABLE , xuserfriend_store.dets).
-define( XUSERTABLE , xuser_store.dets).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External APIs
%% --------------------------------------------------------------------
-export([start/1, start/2, touch/1, set_status/2, get_status/1, get_xuid/1, 
		 add_friend/2, remove_friend/2, is_friend/2, get_friends/1, 
		 get_state/1, delete/1, notify/2, move/2]).

%% --------------------------------------------------------------------
%% gen_server Callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% --------------------------------------------------------------------
%% Module State
%% --------------------------------------------------------------------
-record(xuserstate, {xuserstore, xuid, status, sessions, friends, created, updated, isproxy = false, target}).
%-record(xuser_record,{xuid,created}).
-record(xuserfriend,{xuid, friendxuid, created, creator, deleted}).

%% ====================================================================
%% External functions
%% ====================================================================

%% start(xuid) returns Pid

start(Xuid) when erlang:is_binary(Xuid) ->
	gen_server:start_link(?MODULE,Xuid, []).

start(Xuid, Node) when erlang:is_binary(Xuid) ->
	%% start process on remote node
	{ok, Pid} = rpc:call(Node, gen_server, start_link, [?MODULE, Xuid, []] ),
	{ok, Pid};

start(State, Node) when erlang:is_record(State, xuserstate) ->
	{ok, Pid} = rpc:call(Node, gen_server, start_link, [?MODULE, State, []] ),
	{ok, Pid}.

delete(XuserPid) when erlang:is_pid(XuserPid) ->
	gen_server:call(XuserPid, delete).

%% touch(pid()) returns ok
touch(XuserPid) when erlang:is_pid(XuserPid) ->
	gen_server:call(XuserPid, touch).

notify(XuserPid, Message) when erlang:is_pid(XuserPid)->
	gen_server:cast(XuserPid, {notify, Message}).

%% set_status(Status) returns Current Status
set_status(XuserPid, Status) when erlang:is_pid(XuserPid), erlang:is_atom(Status) ->
	gen_server:call(XuserPid, {set_status, Status}).

%% get_status(Status) returns Current Status
get_status(XuserPid) when erlang:is_pid(XuserPid) ->
	gen_server:call(XuserPid, {get_status}).

%% get_xuid(XuserPid) returns Xuser XUID
get_xuid(XuserPid) when erlang:is_pid(XuserPid) ->
	gen_server:call(XuserPid, get_xuid).

get_state(XuserPid) when erlang:is_pid(XuserPid) ->
	gen_server:call(XuserPid, get_state).


%% add_session(Session) returns Session PID
%% remove_session(Session) returns {ok} or {error}
%% get_sessions() returns Sessions[]

%% add_friend(FriendXuid) returns Friend
add_friend(XuserPid, FriendXuid) when erlang:is_pid(XuserPid), erlang:is_binary(FriendXuid) ->
	gen_server:call(XuserPid, {add_friend, FriendXuid}).

%% is_friend(FriendXuid) -> bool()
is_friend(XuserPid, FriendXuid) when erlang:is_pid(XuserPid), erlang:is_binary(FriendXuid) ->
	gen_server:call(XuserPid, {is_friend, FriendXuid}).

%% get_friends() -> [pid()]
get_friends(XuserPid) when erlang:is_pid(XuserPid) ->
	gen_server:call(XuserPid, get_friends).

%% remove_friend(FriendXuid) returns ok or error
remove_friend(XuserPid, FriendXuid) when erlang:is_pid(XuserPid), erlang:is_binary(FriendXuid) ->
	gen_server:call(XuserPid, {remove_friend, FriendXuid}).

move(XuserPid, Node) when erlang:is_pid(XuserPid) ->
	gen_server:call(XuserPid, {move, Node}).

%% get_friends() returns Friends[]

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
init(State) when erlang:is_record(State, xuserstate) ->
	{ok, State, ?INACTIVITY_TIMEOUT};

init(Xuid) when erlang:is_binary(Xuid) ->
	% DETS lookup
	% should also do some partitioning here since 
	% Mnesia and DETS partitions are limited to 2gb per file
	% process should also close the table correctly
	{ok, XuserStore} = dets:open_file(?XUSERTABLE, [{access, read_write}]),
	Now = erlang:universaltime(),
	UserRow = dets:lookup(XuserStore,Xuid),
	case UserRow of
		[] ->
			% create new state and persist it into DETS
			error_logger:info_msg("User ~p created",[Xuid]),
			State = #xuserstate{xuserstore = XuserStore, xuid = Xuid, status = offline, friends = [], created = Now, updated = Now},
			ok = dets:insert(XuserStore, {Xuid, Now}),
    		{ok,State, ?INACTIVITY_TIMEOUT}; 
		_ ->
			% rehydrate and touch old state
			error_logger:info_msg("User ~p loaded",[Xuid]),
			{_Xuid, Created} = hd(UserRow),
			
			% load friends
			FriendXuids = load_friends(Xuid),
			
			State = #xuserstate{xuserstore=XuserStore,xuid = Xuid, status = offline, friends = FriendXuids, created = Created, updated = Now},
			{ok, State, ?INACTIVITY_TIMEOUT}
	end.
		

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
handle_call(Msg, _From, State) when State#xuserstate.isproxy =:= true ->
	Reply = gen_server:call(State#xuserstate.target, Msg),
	{ok, Reply, State, ?PROXY_TIMEOUT};

handle_call({move,Node}, _From, State) ->
	%% Spawn new process
	{ok,Pid} = xuser:start(State, Node),
	NewState = State#xuserstate{updated = erlang:universaltime(), isproxy = true, target=Pid},
    {reply, {ok,Pid}, NewState, ?PROXY_TIMEOUT};

handle_call(touch, _From, State) ->
	NewState = State#xuserstate{updated = erlang:universaltime()}, 
    {reply, ok, NewState, ?INACTIVITY_TIMEOUT};

handle_call(delete, _From, State) ->
	{stop, delete, ok, State};

handle_call({set_status, Status}, _From, State) when State#xuserstate.isproxy =:= false ->
	NewState = State#xuserstate{status = Status, updated = erlang:universaltime()},
	{reply, { ok, NewState#xuserstate.status }, NewState};

handle_call({get_status}, _From, State) ->
	{reply, { ok, State#xuserstate.status }, State};

handle_call(get_state, _From, State) ->
	{reply, { ok, state_to_proplist(State) }, State};

handle_call(get_xuid, _From, State) ->
	{reply, { ok, State#xuserstate.xuid }, State};

handle_call({add_friend, FriendXuid}, _From, State) ->
	FriendsList = State#xuserstate.friends,
	IsMember = lists:member(FriendXuid, FriendsList), 
	case IsMember of
		false -> 
			error_logger:info_msg("Adding FriendXuser(~p) to Xuser(~p)",[FriendXuid]),
			{ok, XuserFriends} = dets:open_file(?XUSERFRIENDTABLE,[{access, read_write}]),
			dets:insert(XuserFriends, #xuserfriend { xuid = State#xuserstate.xuid, friendxuid = FriendXuid, created=erlang:universaltime(), deleted=false}),
			dets:close(XuserFriends),
			NewState = State#xuserstate{friends = State#xuserstate.friends ++ [FriendXuid], updated = erlang:universaltime()},
			{reply, ok, NewState};
		true -> 
			{reply, already_exists, State}
	end;

handle_call({remove_friend, FriendXuid}, _From, State) ->
	case lists:member(FriendXuid, State#xuserstate.friends) of
		false -> 
			error_logger:info_msg("Removing FriendXuser(~p) from Xuser(~p)",[FriendXuid]),
			{ok, XuserFriendsTable} = dets:open_file(?XUSERFRIENDTABLE,[{access, read_write}]),
			% TODO: Change delete to just flag deleted for later cleanup out-of-band
			dets:delete(XuserFriendsTable, #xuserfriend {xuid = State#xuserstate.xuid, friendxuid=FriendXuid, created='_', deleted='_'}),
			dets:close(XuserFriendsTable),
			NewState = State#xuserstate{friends = lists:delete(FriendXuid,State#xuserstate.friends), updated = erlang:universaltime()},
			{reply, ok, NewState};
		true -> 
			{reply,ok, State}
	end;
	

handle_call({is_friend, FriendXuid}, _From, State) ->
	?assert(is_binary(FriendXuid)),
	{reply, {ok, lists:member(FriendXuid, State#xuserstate.friends)}, State};

handle_call(get_friends, _From, State) ->
	{reply, {ok, State#xuserstate.friends}, State};

handle_call(_, _From, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(Msg, State) when State#xuserstate.isproxy =:= true ->
	gen_server:cast(State#xuserstate.target, Msg, ?PROXY_TIMEOUT);

handle_cast({notify, Message}, State) ->
	io:format("~p~s", Message),
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

handle_info(timeout, State) when State#xuserstate.isproxy =:= false ->
	error_logger:info_report("XUser Process Timedout (xuid:~p)", [State#xuserstate.xuid]),
	{stop, normal, State};

handle_info(timeout, State) when State#xuserstate.isproxy =:= true ->
	error_logger:info_report("XUser Proxy Timeout (xuid:~p)", [State#xuserstate.xuid]),
	{stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	% close dets table
	dets:close(State#xuserstate.xuserstore),
	error_logger:info_report("XUser ~p terminated ~p", [State#xuserstate.xuid, Reason]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

load_friends(Xuid) ->
	error_logger:info_msg("Loading friends for XUser(~p)",[Xuid]),
	{ok, XuserFriends} = dets:open_file(?XUSERFRIENDTABLE,[{access, read}]),
	FriendsRowSet = dets:select(XuserFriends, 
								[{
									#xuserfriend { xuid = Xuid, friendxuid = '$1', created='_', deleted='_'},
								 	[],
								 	['$1']
								}]),
	dets:close(XuserFriends),
	FriendsRowSet.

state_to_proplist(State) ->
	[
	 	{xuid, State#xuserstate.xuid},
		{status, State#xuserstate.status},
		{created,  list_to_binary(httpd_util:rfc1123_date(State#xuserstate.created))},
		{updated, list_to_binary(httpd_util:rfc1123_date(State#xuserstate.updated))}
	].

%% --------------------------------------------------------------------
%%% Unit Tests
%% --------------------------------------------------------------------

setup_test() ->
	ok.

cleanup_test() ->
	
	{ok, XuserTable} = dets:open_file(?XUSERTABLE,[{access, read_write}]),
	ok = dets:delete_all_objects(XuserTable),
	dets:close(XuserTable),

	{ok, XuserFriendsTable} = dets:open_file(?XUSERFRIENDTABLE,[{access, read_write}]),
	ok = dets:delete_all_objects(XuserFriendsTable),
	dets:close(XuserFriendsTable),
	ok.

get_status_test() ->
	{ok,Pid} = xuser:start(<<"get_status_test">>),
	% status defaults to offline
	?assert(is_pid(Pid)),
	?assertEqual({ok, offline}, xuser:get_status(Pid)),

	% transition and validate
	xuser:set_status(Pid, online),
	?assertEqual({ok, online}, xuser:get_status(Pid)),

	% transition and validate
	xuser:set_status(Pid, offline),
	?assertEqual({ok, offline}, xuser:get_status(Pid)).

set_status_test() ->
	{ok,Pid} = xuser:start(<<"set_status_test">>),
	xuser:set_status(Pid, online),
	?assertEqual({ok, online }, xuser:get_status(Pid)).

get_xuid_test() ->
	{ok,Pid} = xuser:start(<<"get_xuid_test_xuid">>),
	?assertEqual( {ok, <<"get_xuid_test_xuid">>}, xuser:get_xuid(Pid) ).

get_state_test() ->
	{ok,Pid} = xuser:start(<<"get_state">>),
	{ok, Properties} = xuser:get_state(Pid),
	?assert(erlang:is_list(Properties)),
	{xuid,Xuid} = proplists:lookup(xuid, Properties),
	?assertEqual(Xuid,<<"get_state">>).

add_friend_test() ->
	Xuser = <<"add_friend_test">>,
	{Response, Pid} = xuser:start(Xuser),
	?assertEqual(ok, Response),
	?assert(is_pid(Pid)),
	
	% assert no friends
	?assertEqual({ok, false}, xuser:is_friend(Pid, <<"bogus">>)),
	{ok,Friends0} = xuser:get_friends(Pid),
	?assert(is_list(Friends0)),
	?assertEqual(0, length(Friends0)),

	FriendXuid1 = <<"add_friend_test Friend">>,
	AddResponse1 = xuser:add_friend(Pid, FriendXuid1),
	?assertEqual(ok, AddResponse1),
	?assertEqual({ok, true}, xuser:is_friend(Pid, FriendXuid1)),
	?assertEqual({ok, false}, xuser:is_friend(Pid, <<"bogus">>)),
	{ok,Friends1} = xuser:get_friends(Pid),
	?assert(is_list(Friends1)),
	?assertEqual(1, length(Friends1)),

	FriendXuid2 = <<"add_friend_test Friend2">>,
	AddResponse2 = xuser:add_friend(Pid, FriendXuid2),
	?assertEqual(ok, AddResponse2),
	?assertEqual({ok, true}, xuser:is_friend(Pid, FriendXuid1)),
	?assertEqual({ok, true}, xuser:is_friend(Pid, FriendXuid2)),
	?assertEqual({ok, false}, xuser:is_friend(Pid, <<"bogus">>)),
	{ok,Friends2} = xuser:get_friends(Pid),
	?assert(is_list(Friends2)),
	?assertEqual(2, length(Friends2)).

%% Tests early exit when a friend already exists in friends list.
add_existing_friend_test() ->
	Xuser = <<"add_friend_test">>,
	{Response, Pid} = xuser:start(Xuser),
	?assertEqual(ok, Response),
	?assert(is_pid(Pid)),

	FriendXuid1 = <<"add_friend_test Friend">>,
	AddResponse1 = xuser:add_friend(Pid, FriendXuid1),
	?assertEqual(ok, AddResponse1),
	?assertEqual({ok, true}, xuser:is_friend(Pid, FriendXuid1)),
	?assertEqual({ok, false}, xuser:is_friend(Pid, <<"bogus">>)),
	{ok,Friends1} = xuser:get_friends(Pid),
	?assert(is_list(Friends1)),
	?assertEqual(1, length(Friends1)),

	AddResponse2 = xuser:add_friend(Pid, FriendXuid1),
	?assertEqual(already_exists, AddResponse2),
	?assertEqual({ok, true}, xuser:is_friend(Pid, FriendXuid1)),
	?assertEqual({ok, false}, xuser:is_friend(Pid, <<"bogus">>)),
	{ok,Friends2} = xuser:get_friends(Pid),
	?assert(is_list(Friends2)),
	?assertEqual(1, length(Friends2)).

is_friend_test() ->
	
	UserXuid = <<"is_friend">>,
	
	% Create user
	{Response, UserPid} = xuser:start(UserXuid),
	?assertEqual(ok, Response),
	?assert(is_pid(UserPid)),
	
	% Add Friend
	FriendXuid = <<"is_friend Friend">>,
	AddResponse = xuser:add_friend(UserPid, FriendXuid),
	?assertEqual(ok, AddResponse),
	
	% Check Friends List
	{ok,Friends} = xuser:get_friends(UserPid),
	?assert(is_list(Friends)),
	?assertEqual(1, length(Friends)),
	?assertEqual(FriendXuid, lists:nth(1, FriendXuid)),
	
	% Check "is_friend" status with positive and negative values
	?assertEqual({ok, true}, xuser:is_friend(UserPid, FriendXuid)),
	?assertEqual({ok, false}, xuser:is_friend(UserPid, <<"bogus">>)),

	% Cleanup Friend List
	xuser:remove_friend(UserPid,FriendXuid),
	{ok,CleanFriends} = xuser:get_friends(UserPid),
	?assert(is_list(CleanFriends)),
	?assertEqual(1, length(CleanFriends)),
	?assertEqual(FriendXuid, lists:nth(1, CleanFriends)).

remove_friend_test() ->
	LookupValue = <<"remove_friend_test">>,
	{Response, Pid} = xuser:start(LookupValue),
	?assertEqual(ok, Response),
	FriendLookupValue = <<"remove_friend_test Friend">>,
	AddResponse = xuser:add_friend(Pid, FriendLookupValue),
	?assertEqual(ok, AddResponse),
	?assertEqual({ok, true}, xuser:is_friend(Pid, FriendLookupValue)),
	?assertEqual({ok, false}, xuser:is_friend(Pid, <<"bogus">>)),
	RemoveResponse = xuser:remove_friend(Pid, FriendLookupValue),
	?assertEqual(ok, RemoveResponse),
	?assertEqual({ok, false}, xuser:is_friend(Pid, FriendLookupValue)).

get_friends_test() ->
	LookupValue = <<"get_friends_test">>,
	{Response, Pid} = xuser:start(LookupValue),
	?assertEqual(ok, Response),
	FriendLookupValue1 = <<"get_friends_test Friend1">>,
	FriendLookupValue2 = <<"get_friends_test Friend2">>,
	AddResponse1 = xuser:add_friend(Pid, FriendLookupValue1),
	?assertEqual(ok, AddResponse1),
	{ok, GetResponse1} = xuser:get_friends(Pid),
	?assertEqual(1, length(GetResponse1)),
	?assertEqual(FriendLookupValue1, lists:nth(1, GetResponse1)),
	AddResponse2 = xuser:add_friend(Pid, FriendLookupValue2),
	?assertEqual(ok, AddResponse2),
	{ok, GetResponse2} = xuser:get_friends(Pid),
	?assertEqual(2, length(GetResponse2)),
	?assertEqual(FriendLookupValue1, lists:nth(1, GetResponse2)),
	?assertEqual(FriendLookupValue2, lists:nth(2, GetResponse2)).