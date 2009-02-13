%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   :  3 Apr 2008 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Re-worked :  7 Feb 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.     :  Implementation of OpenID v1.1 
%%%-------------------------------------------------------------------
-module(eopenid_srv).

-behaviour(gen_server).

-export([start_link/0
         ,put_assoc_handle/2
         ,get_assoc_handle/1
        ]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(s, {}).

%%% --------------------------------------------------------------------
%%% @spec put_assoc_handle( string() , string() ) -> ok.
%%%
%%% @doc Store the Association Handle for a certain Provider.
%%% @end
%%% --------------------------------------------------------------------
-spec put_assoc_handle( string() , string() ) -> ok.

put_assoc_handle(Provider, AssocHandle) ->
    ok = gen_server:call(?SERVER, {put_assoc_handle, Provider, AssocHandle}).

%%% --------------------------------------------------------------------
%%% @spec gut_assoc_handle( string() ) -> {ok,AssocHandle} | 
%%%                                       {error, not_found}.
%%%
%%% @doc Get the Association Handle for a certain Provider.
%%% @end
%%% --------------------------------------------------------------------
-spec get_assoc_handle( string() ) -> {ok,string()} | {error, not_found}.

get_assoc_handle(Provider) ->
    case ets:lookup(?TABLE, Provider) of
        [{_,AssocHandle}] -> {ok, AssocHandle};
        []                -> {error, not_found}
    end.

            

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?TABLE, [named_table, protected]),
    {ok, #s{}}.

handle_call({put_assoc_handle, Provider, AssocHandle}, _From, State) ->
    ets:insert(?TABLE, {Provider, AssocHandle}),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

