-module(udon_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(SERVER, ?MODULE).
%% ===================================================================
%% API functions
%% ===================================================================

start_link(UdonConnectionArgs) ->
	ets:new(udon_nodeid_client_maps, [set, public, named_table]),
	ets:new(udon_nodeid_key_cache, [set, public, named_table]),
	supervisor:start_link({local, ?SERVER}, ?MODULE, [UdonConnectionArgs]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([{UdonInstances, UdonClientNamePrefix}]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Restart = permanent,
	Shutdown = 2000,
	Type = worker,

	Children = [{udon_client_name(UdonClientNamePrefix, UdonNodeId),
		{udon_client, start_link, [{UdonNodeId,{UdonHostName, UdonPort, UdonSize, UdonMaxOverflow}}]},
		Restart, Shutdown, Type, [udon_client]} || {UdonNodeId,{UdonHostName, UdonPort, UdonSize, UdonMaxOverflow}} <- UdonInstances],

	{ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
udon_client_name(UdonClientNamePrefix, UdonNodeId) ->
	list_to_atom(UdonClientNamePrefix ++ atom_to_list(UdonNodeId)).
