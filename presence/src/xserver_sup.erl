%%% -------------------------------------------------------------------
%%% Author  : stephbu
%%% Description : XServer Supervisor Process
%%%
%%% Created : Dec 7, 2010
%%% -------------------------------------------------------------------
-module(xserver_sup).
-behaviour(supervisor).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/1]). 

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(Args) ->
	error_logger:info_msg("Starting XServer Supervisor"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(Args) ->
	% xuser_finder is the only process that we monitor
	% permanent indicates that it will always be restarted
	% int() - indicates short orderly shutdown timeout before brutal_kill
    XUserFinderServer = {
					xuser_manager,
					{xuser_manager,start_link,Args},
	   		   		permanent,
					1000,
					worker,
					[xuser_manager]
				},
	XHttpServer = {
					xhttp,
					{xhttp, start_link, Args},
					permanent,
					1000,
					worker,
					[xhttp]
				},
	SupervisedChildren = [XUserFinderServer, XHttpServer],

	% one_for_one will only restart supervised processes that die without touching other processes
	RestartStrategy = {one_for_one, 0, 1},

    {ok, {RestartStrategy, SupervisedChildren} }.

start_test() ->
	case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of 
		{ok,Pid} ->
			unlink(Pid);
		Msg ->
			error_logger:error_msg("~p", Msg)
	end.
		
