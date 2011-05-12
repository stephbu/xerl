%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{
	application, 
	xserver,
	[
		{description, "XServer"},
		{vsn, "0.1.0"},
		{modules, [xserver_app,xserver_sup,xuser_manager,xuser, xhttp, xmodule]},
		{registered, []},
		{applications, [kernel, stdlib]},
		{mod, {xserver_app, []}}
	]
}.