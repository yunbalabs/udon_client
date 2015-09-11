-module(udon_client_app).

%% Application callbacks
-export([start/1, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(UdonInstances) ->  %% UdonInstances = [{udon1, {"localhost", 6380, 10, 20}}]
	init_logic_udon_client_module(UdonInstances).

stop(_State) ->
    ok.

init_logic_udon_client_module(UdonInstances) ->
	case ets:info(udon_nodeid_client_maps) of
		undefined ->
			ets:new(udon_nodeid_client_maps, [set, public, named_table]),
			ets:new(udon_nodeid_key_cache, [set, public, named_table]),

			lists:foreach(fun ({UdonNodeId, {UdonHostName, UdonPort, UdonSize, UdonMaxOverflow}}) ->
				ets:insert(udon_nodeid_client_maps, {UdonNodeId, {UdonHostName, UdonPort, UdonSize, UdonMaxOverflow}}),
				udon_client:init({UdonNodeId, {UdonHostName, UdonPort, UdonSize, UdonMaxOverflow}})
			end, UdonInstances),
			ok;
		_ ->
			started
	end.