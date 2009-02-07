%%%-------------------------------------------------------------------
%%% Created   :  3 Apr 2008 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Re-worked :  7 Feb 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.     :  Implementation of OpenID v1.1 
%%%-------------------------------------------------------------------
-module(eopenid_v1).

-export([discover/1]).

-import(eopenid_lib, 
        [http_get/1
         ,http_path_norm/1
        ]).

%%% --------------------------------------------------------------------
%%% @spec discover(ClaimedId :: list()) -> dict()
%%%
%%% @doc Performs an OpenID discovery to find out the the 
%%%      provider and the (optional) delegated ID.
%%% @end
%%% --------------------------------------------------------------------
discover(ClaimedId) ->
    NormId = http_path_norm(ClaimedId),
    Reply = http_get(NormId).


