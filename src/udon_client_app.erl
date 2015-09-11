-module(udon_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	UdonConnectionArgs = init_logic_udon_client_module(),
    udon_client_sup:start_link(UdonConnectionArgs).

stop(_State) ->
    ok.

init_logic_udon_client_module() ->
	UdonInstances = application:get_env(udon_client, udon_instances, [{udon1, {"localhost", 6380, 10, 20}}]),
	UdonClientNamePrefix = application:get_env(udon_client, udon_client_name_prefix, "udon_client_"),
	{UdonInstances, UdonClientNamePrefix}.
