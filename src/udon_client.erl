%%%-------------------------------------------------------------------
%%% @author Zhengyinyong
%%% @copyright (C) 2015, Yunba
%%% @doc
%%%
%%% @end
%%% Created : 01. 九月 2015 上午11:31
%%%-------------------------------------------------------------------
-module(udon_client).
-author("Zhengyinyong").

-compile({parse_transform, lager_transform}).

-include_lib("elog/include/elog.hrl").

-export([init/1,
	exec_command/3]).

%%%===================================================================
%%% API
%%%===================================================================
init({UdonNodeId, {UdonHostName, UdonPort, UdonSize, UdonMaxOverflow}}) ->
	case eredis_pool:create_pool(UdonNodeId, {UdonSize, UdonMaxOverflow}, UdonHostName, UdonPort) of
		{ok, Pid} ->
			?INFO("connecting udon => node: ~p, host: ~p, port: ~p, client_pid: ~p~n",
				  [UdonNodeId, UdonHostName, UdonPort, Pid]),
			{ok, Pid};
		{error, Reason} ->
			?ERROR("cannot connect udon because of ~p => node: ~p, host: ~p, port: ~p~n",
				   [UdonNodeId, UdonHostName, UdonPort, Reason]),
			{stop, Reason}
	end.

exec_command(_UdonNodeId, {Command, {Bucket, Key}, Args}, 0) ->
	?ERROR("udon client error for => command ~p, key {~p,~p} args ~p because of forward_error_overtimes~n",
		[Command, Bucket, Key, Args]),
	{error, forward_error_overtimes};
exec_command(UdonNodeId, {Command, {Bucket, Key}, Args}, TryTimes) ->
	Key1 = [Bucket, Key],
	case eredis_pool:q(UdonNodeId, [Command, Key1 | Args]) of
		{ok, UdonResponse} ->
			ets:insert(udon_nodeid_key_cache, {{Bucket, Key}, UdonNodeId}),
			{ok, UdonResponse};
		{error, Reason} ->
			NewNodeId = get_nodeid_from_forward_error(Reason),
			case NewNodeId /= none of
				true ->
					exec_command(NewNodeId, {Command, {Bucket, Key}, Args}, TryTimes - 1);
				false ->
					?ERROR("udon client error for => command ~p, key {~p,~p} args ~p because of ~p~n",
						[Command, Bucket, Key, Args, Reason]),
					{error, Reason}
			end
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_nodeid_from_forward_error(Reason) ->
	try
	    case jiffy:decode(Reason) of
			{[{<<"forward">>, UdonNodeIdBin}]} ->
				erlang:binary_to_atom(UdonNodeIdBin, latin1);
			_Else ->
				none
		end
	catch
	    Type:Reason1 ->
			?ERROR("jiffy exception for Type = ~p, Reason = ~p~n", [Type, Reason1]),
			none
	end.