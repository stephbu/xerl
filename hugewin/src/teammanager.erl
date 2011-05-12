%% Author: stephbu
%% Created: Mar 14, 2011
%% Description: TODO: Add description to teammanager
-module(teammanager).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([create_team/1]).

%%
%% API Functions
%%

create_team(Name) ->
	%% Insert into database
	Name.

%%
%% Local Functions
%%

create_team_test() ->
	create_team(<<"TestTeam">>).
