%%% -------------------------------------------------------------------
%%% Author  : stephbu
%%% Description : XPresence Application Process
%%%
%%% Created : Dec 7, 2010
%%% -------------------------------------------------------------------
-module(xserver_app).
-behaviour(application).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([start/2, stop/1]).

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, StartArgs) ->
    case xserver_sup:start_link(StartArgs) of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.