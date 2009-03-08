%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   :  3 Apr 2008 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Re-worked :  7 Feb 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.     :  Implementation of OpenID v2.0
%%%
%%%-------------------------------------------------------------------
-module(eopenid_v2).

-export([discover/1
         ,discover/2
        ]).

-ignore_xref([{discover,1}]).

-import(eopenid_lib, 
        [new/0
         ,in/3
         ,http_get/1
         ,http_get/2
         ,http_post/4
         ,http_path_norm/1
         ]).

-include("eopenid_typespecs.hrl").
-include("eopenid_debug.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%%% --------------------------------------------------------------------
%%% @spec discover(ClaimedId :: list()) -> {ok,dict()} | {Type,Error}.
%%%
%%% @doc Performs an OpenID discovery to find out the the 
%%%      provider and the (optional) delegated ID.
%%%      Returns an updated Dict.
%%% @end
%%% --------------------------------------------------------------------
-spec discover( string() ) -> {ok,dict()} | {etype(),error()}.
    
discover(ClaimedId) when is_list(ClaimedId) ->
    discover(ClaimedId, new()).

discover(ClaimedId, Dict0) when is_list(ClaimedId) ->
    XrdsContent = "application/xrds+xml",
    Hdrs = [{"Accept", XrdsContent}],
    NormId = http_path_norm(ClaimedId),
    Dict = in("openid2.claimed_id", ClaimedId, Dict0),
    try
        {ok, {{_,200,_}, RHdrs, Body}} = http_get(NormId, Hdrs),
        case proplists:get_value("content-type", RHdrs) of
            XrdsContent -> discover_xrds(Body);
            Else        -> discover_html(Body)
        end
    catch
         _:Error ->       
            ?edbg("discover failed: ~p~n", [erlang:get_stacktrace()]),
            {error, Error}
    end.


-define(xelem(N,As,C), #xmlElement{name       = N, 
                                   attributes = As,
                                   content    = C}).

-define(xattr(N,V), #xmlAttribute{name  = N, 
                                  value = V}).

-define(xtxt(Tag,Val), #xmlText{parents = [{Tag,_}|_],
                                value   = Val}).

discover_xrds(Body) ->
    {Xml,_} = xmerl_scan:string(Body),
    Services = xmerl_xpath:string("//Service", Xml),
    [parse(S) || S <- Services].

parse(?xelem(_,_,Exports)) ->
    [{Tag,Val} || 
        ?xelem(_,_,Products) <- Exports,
        ?xtxt(Tag,Val)       <- Products].

discover_html(Body) ->
    tbd.


