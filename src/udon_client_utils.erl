%%%-------------------------------------------------------------------
%%% @author Zhengyinyong
%%% @copyright (C) 2015, Yunba
%%% @doc
%%%
%%% @end
%%% Created : 11. 九月 2015 上午11:39
%%%-------------------------------------------------------------------
-module(udon_client_utils).
-author("Zhengyinyong").

%% API
-export([to_string/1]).

to_string(Data) ->
	if
		is_binary(Data) ->
			binary_to_list(Data);
		is_integer(Data) ->
			integer_to_list(Data);
		is_atom(Data) ->
			atom_to_list(Data);
		true ->
			Data
	end.
