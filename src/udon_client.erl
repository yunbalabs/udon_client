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

-behaviour(gen_server).

-compile({parse_transform, lager_transform}).

-include_lib("elog/include/elog.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {udon_node_id, udon_host_name, udon_port, client_pid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term()) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link({UdonNodeId,{UdonHostName, UdonPort, UdonSize, UdonMaxOverflow}}) ->
	gen_server:start_link(?MODULE, [{UdonNodeId,{UdonHostName, UdonPort, UdonSize, UdonMaxOverflow}}], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([{UdonNodeId,{UdonHostName, UdonPort, UdonSize, UdonMaxOverflow}}]) ->
	case eredis_pool:create_pool(udon_pool_name(UdonHostName),{UdonSize, UdonMaxOverflow}, UdonHostName, UdonPort) of
		{ok, Pid} ->
			?INFO("connecting udon => node: ~p, host: ~p, port: ~p, client_pid: ~p~n",
				  [UdonNodeId, UdonHostName, UdonPort, Pid]),
			ets:insert(udon_nodeid_client_maps, {UdonNodeId, self()}),
			{ok, #state{udon_node_id = UdonNodeId, udon_host_name = UdonHostName, udon_port = UdonPort, client_pid = Pid}};
		{error, Reason} ->
			?ERROR("cannot connect udon because of ~p => node: ~p, host: ~p, port: ~p~n",
				   [UdonNodeId, UdonHostName, UdonPort, Reason]),
			{stop, Reason}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call({Command, {Bucket, Key}, Args}, _From,
		    #state{udon_node_id = UdonNodeId, udon_host_name = UdonHostName}=State) ->
	Key1 = [Bucket, Key],
	Result = case eredis_pool:q(udon_pool_name(UdonHostName), [Command, Key1| Args]) of
				 {ok, UdonResponse} ->
					 ets:insert(udon_nodeid_key_cache, {{Bucket, Key}, UdonNodeId}),
					 {ok, UdonResponse};
				 {error, Reason} ->
					 NodeId = get_nodeid_from_forward_error(Reason),
					 case NodeId /= none of
						 true ->
							 NewClientPid = udon_storage:get_pid_from_nodeid(NodeId),
							 case  NewClientPid /= none of
								 true ->
									 gen_server:call(NewClientPid, {forward_error, 1, Command, {Bucket, Key}, Args});
								 false ->
									 ?ERROR("udon client error for => command ~p, key {~p,~p} args ~p because of ~p~n",
										    [Command, Bucket, Key, Args, Reason]),
									 {error, Reason}

							 end;
						 false ->
							 ?ERROR("udon client error for => command ~p, key {~p,~p} args ~p because of ~p~n",
								    [Command, Bucket, Key, Args, Reason]),
							 {error, Reason}
					 end
			 end,
	{reply, Result, State};

handle_call({forward_error, RetryTimes, Command, {Bucket, Key}, Args}, _From,
		    #state{udon_node_id = UdonNodeId, udon_host_name = UdonHostName}=State) ->
	Key1 = [Bucket, Key],
	case RetryTimes =:= 3 of
		true ->
			{reply, {error, forward_error_overtimes}, State};
		false ->
			Result = case eredis_pool:q(udon_pool_name(UdonHostName), [Command, Key1| Args]) of
						 {ok, UdonResponse}  ->
							 ets:insert(udon_nodeid_key_cache, {{Bucket, Key}, UdonNodeId}),
							 {ok, UdonResponse};
						 {error, Reason} ->
							 NodeId = get_nodeid_from_forward_error(Reason),
							 case NodeId /= none of
								 true ->
									 NewClientPid = udon_storage:get_pid_from_nodeid(NodeId),
									 case  NewClientPid /= none of
										 true ->
											 gen_server:call(NewClientPid, {forward_error, RetryTimes + 1, Command, {Bucket, Key}, Args});
										 false ->
											 ?ERROR("udon forward error for => retry_times ~p, command ~p, key {~p,~p} args ~p error because of ~p~n",
												    [RetryTimes, Command, Bucket, Key, Args, Reason]),
											 {error, Reason}

									 end;
								 false ->
									 ?ERROR("udon forward error for => retry_times ~p, command ~p, key {~p,~p} args ~p error because of ~p~n",
										    [RetryTimes, Command, Bucket, Key, Args, Reason]),
									 {error, Reason}
							 end
					 end,
			{reply, Result, State}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
		Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

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

udon_pool_name(UdonHostName) ->
	list_to_atom(udon_client_utils:to_string(UdonHostName) ++ "_udon_pool").
