%% Author: stephbu
%% Description: Deploys all BEAM files in from a specified directory to nodes.
%% @doc Deploys BEAM files from a specified path to nodes.
%%
%% Usage:
%%
%%    1> ok = xdeploy:deploy_node(<<"c:\appsrc\">>,Node()).
-module(xmodule).

-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([deploy_node/2, deploy_nodes/2]).

%%
%% API Functions
%%
-spec(deploy_nodes/2::(binary(), list()) -> atom() | tuple()).
deploy_nodes(SourcePath, Nodes) ->
	% check path exists
	case list_modules(SourcePath) of
		{ok, Files} ->
		  	DeployFun = fun(Node) ->
			% check node connection
			case net_kernel:connect_node(Node) of
				true ->
					deploy_beams(Files, Node),
					ok;
				false ->
					{error, no_node_connection};
				ignored ->
					{error, net_kernel_not_running}
			end
		end,
		lists:foreach(DeployFun, Nodes);
		Error -> 
			erlang:throw(Error)
	end,
	ok.

-spec(deploy_node/2::(binary(), node()) -> atom() | tuple()).
deploy_node(SourcePath, Node) ->
	deploy_nodes(SourcePath, [Node]).

-type name() :: [string() | atom()].
-spec(list_modules/1::(name()) -> atom() | list()).
list_modules(SourcePath) ->
	find:files(SourcePath, "*.beam", true).

-spec(deploy_beams/2::(node(), list()) -> ok).
deploy_beams(Node, Modules) ->
	DeployFun = fun(Module) -> deploy_beam(Node, Module) end,
	lists:foreach(DeployFun, Modules),
	ok.

deploy_beam(_Node, _Module) ->
	ok.

list_modules_test() ->
	list_modules("d:/dev/xerl/").