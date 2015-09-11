%%%-------------------------------------------------------------------
%%% @author Zhengyinyong
%%% @copyright (C) 2015, Yunba
%%% @doc
%%%
%%% @end
%%% Created : 01. 九月 2015 下午4:01
%%%-------------------------------------------------------------------
-module(udon_storage).
-author("Zhengyinyong").

-compile({parse_transform, lager_transform}).

-include_lib("elog/include/elog.hrl").

%% API
-export([execuate/3, execuate/4, get_pid_from_nodeid/1, get_nodeid_randomly/0]).

execuate(Command, {Bucket, Key}, Args) ->
	BucketStr = udon_client_utils:to_string(Bucket) ++ ",",
	KeyStr = udon_client_utils:to_string(Key),
	RequestNodeId = case ets:lookup(udon_nodeid_key_cache, {BucketStr, KeyStr}) of
						[{BucketStr, KeyStr}, NodeId] ->
							NodeId;
						_Else ->
							get_nodeid_randomly()
					end,
	request_for_udon(RequestNodeId, {Command, {BucketStr, KeyStr}, Args}).

execuate(NodeId, Command, {Bucket, Key}, Args) ->
	BucketStr = udon_client_utils:to_string(Bucket) ++ ",",
	KeyStr = udon_client_utils:to_string(Key),
	request_for_udon(NodeId, {Command, {BucketStr, KeyStr}, Args}).

get_pid_from_nodeid(NodeId) ->
	case ets:lookup(udon_nodeid_client_maps, NodeId) of
		[{NodeId, Pid}] ->
			Pid;
		_Else ->
			none
	end.

get_nodeid_randomly() ->
	NodeIdClientMapsLists = ets:tab2list(udon_nodeid_client_maps),
	case NodeIdClientMapsLists /= [] of
		true ->
			{NodeId, _Pid} = lists:nth(random:uniform(length(NodeIdClientMapsLists)), NodeIdClientMapsLists),
			NodeId;
		false ->
			?ERROR_MSG("udon_nodeid_client_maps is empty"),
			none
	end.

request_for_udon(RequestNodeId, RequestArgs) ->
	Pid = get_pid_from_nodeid(RequestNodeId),
	case Pid /= none of
		true ->
			{Command, {Bucket, Key}, Args} = RequestArgs,
			case gen_server:call(Pid, {Command, {Bucket, Key}, Args}) of
				{ok, UdonResponse} ->
					{ok, UdonResponse};
				{error, Reason} ->
					?ERROR("request for ~p error because of ~p, delete from udon_nodeid_key_cache~n", [RequestNodeId, Reason]),
					ets:delete(udon_nodeid_key_cache, {Bucket, Key}),
					{error, Reason}
			end;
		false ->
			?ERROR("~p is not exist~n", [RequestNodeId]),
			{error, no_such_nodeid}
	end.
