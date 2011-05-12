%%% -------------------------------------------------------------------
%%% Author  : stephbu
%%% Description :
%%%
%%% Created : Dec 9, 2010
%%% -------------------------------------------------------------------
-module(xhttp).
-behaviour(gen_server).


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, stop/0, loop/1]).

%% Record structures
-record(state, {httpserver}).


%% ====================================================================
%% External functions
%% ====================================================================
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Args) ->
    Loop = fun (Req) ->
                   ?MODULE:loop(Req)
           end,
    {_Status,Pid} = mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Args]),
	State = #state{httpserver=Pid},
    {ok, State}.

stop() ->
	gen_server:cast(?MODULE, stop).
	
loop(Req) ->
	try
		
		% Extract Basic Parameters
		PathTokens = string:tokens(Req:get(path),"/"),
		Command = extract_command(PathTokens),
		Method = Req:get(method),
		Entity = list_to_binary(lists:nth(1, PathTokens)),
		Identity = list_to_binary(lists:nth(2, PathTokens)),
		
		%% TODO:  Validate User Parameters
		
		process_request(Req, Method, Entity, Identity, Command)
	
	catch
		throw:Term -> Term; 
		exit:Reason -> 
			error_logger:error_msg(Reason),
			{exit, Reason};
		error:Reason -> 
			Stack = erlang:get_stacktrace(),
			Req:ok({"text/html",[],[Stack]}), 
		  	{error,{Reason,Stack}} 
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

handle_call(_, _From, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(stop, State) ->
	{stop, normal, State};

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
	mochiweb_http:stop(?MODULE),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal Functions
%% ====================================================================

%% Process User Entity Related Requests
process_request(Req, Method, Entity, Identity, Command)  when Entity =:= <<"User">> ->
	
	case Req of
		
		% Get User
		Request when Method =:= 'GET', Command =:= undefined ->
			{_Activation,Xuser} = xuser_finder:get(Identity),
			{ok, XuserProperties} = xuser:get_state(Xuser),
			Json = proplist_to_json(XuserProperties),
			Request:ok({"text/plain",[{"Cache-Control","no-cache"}],[Json]});
		
		% Get User Friends
		Request when Method =:= 'GET', Command =:= <<"Friends">> ->
			{_Activation,Xuser} = xuser_finder:get(Identity),
			{ok,Friends} = xuser:get_friends(Xuser),
			Json = proplist_to_json([{friends, Friends}]),
			Request:ok({"text/plain",[{"Cache-Control","no-cache"}],[Json]});
		
		% Delete User
		Request when Method =:= 'DELETE', Command =:= undefined ->
			ok = xuser:delete(Identity),
			Request:ok({"text/html",[],[<<"OK">>]});
		
		Request when Method =:= 'POST' ->
			Json = json_from_request(Request),
			Value = undefined,
			case Value of
				undefined ->
					Request:respond({500, [{"Content-Type", "text/html"}], <<"Argument not defined - friendxuid">>});
				FriendXuid ->
					{_Activation,Xuser} = xuser_finder:get(Identity),
					ok = xuser:add_friend(Xuser, FriendXuid),
					{ok,Friends} = xuser:get_friends(Xuser),
					Json = proplist_to_json([{friends, Friends}]),
					Request:ok({"text/plain",[{"Cache-Control","no-cache"}],[Json]})
			end;
		
		Request when Method =:= 'POST', Command =:= <<"SetStatus">> ->
			Request:ok("text/plain",[],[<<"Done">>]);
		
		Request ->
			Request:not_found()
	end.

extract_command(PathTokens) ->
	if
		length(PathTokens) == 3 -> 
			Command = list_to_binary(lists:nth(3, PathTokens)),
			true;
		true ->
			Command = undefined,
			true
	end,
	Command.
	

proplist_to_json(PropList) when erlang:is_list(PropList) ->
	mochijson2:encode(PropList).

json_from_request(Req) ->
  RecvBody = Req:recv_body(),
  Data = case RecvBody of
    <<>> -> erlang:list_to_binary("{}");
    Bin -> Bin
  end,
  {struct, Struct} = mochijson2:decode(Data),
  Struct.



%% ====================================================================
%% Unit tests
%% ====================================================================
start_stop_test() ->
	%% Start and stop the server and send it a smoke message a couple of times to:
	%% a) verify it starts
	%% b) verify it stops ok
	%% c) verify it will restart
	
	?debugMsg("Starting 1st time"),
	{ok, MochiWebPid} = xhttp:start([]),
	?assert(erlang:is_pid(MochiWebPid)),
	?assert(erlang:is_process_alive(MochiWebPid)),

	?debugMsg("Sending Smoke Message"),
	
	
	?debugMsg("Stopping 1st time"),
	ok = xhttp:stop(),

	?debugMsg("Starting 2nd time"),
	{ok, MochiWebPid} = xhttp:start([]),
	?assert(erlang:is_pid(MochiWebPid)),
	?assert(erlang:is_process_alive(MochiWebPid)),

	?debugMsg("Stopping 2nd time"),
	ok = xhttp:stop().