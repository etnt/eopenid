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
         ,normalize/1
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
-include_lib("eunit/include/eunit.hrl").


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
    Sort = fun({A1,B1}=A,{A2,B2}=B) ->
                   lkup(priority,A1) =< lkup(priority,A2)
           end,
    lists:sort(Sort,[{parse_attributes(S),parse_body(S)} || S <- Services]).

parse_attributes(?xelem(_,Attributes,_)) ->
    [{Name,Val} || 
        ?xattr(Name,Val)       <- Attributes].

parse_body(?xelem(_,_,Exports)) ->
    [{Tag,Val} || 
        ?xelem(_,_,Products) <- Exports,
        ?xtxt(Tag,Val)       <- Products].

discover_html(Body) ->
    tbd.

lkup(K,L) ->
    lists:keyfind(K,1,L).



%%%
%%% Normalize the Claimed ID according to 7.2 of:
%%% http://openid.net/specs/openid-authentication-2_0.html
%%%
-define(is_xri_char(C), (C == $= orelse
                         C == $@ orelse
                         C == $+ orelse
                         C == $$ orelse
                         C == $! orelse
                         C == $( )).

normalize("xri://"++[H|T]=Rest) when ?is_xri_char(H) ->
    [H|T];
normalize("http://"++Rest) ->
    "http://"++http_normalize(Rest);
normalize("https://"++Rest) ->
    "https://"++http_normalize(Rest);
normalize([X|_]=Rest) when ?is_xri_char(X) ->
    Rest;
normalize(Id) ->
    "http://"++http_normalize(Id).
    

http_normalize(Str0) ->
    [Str|_] = string:tokens(Str0, "#"),
    case string:tokens(Str,"/") of
        [X] -> X++"/";
        _   -> Str
    end.
    

-ifdef(EUNIT).

%%%
%%% See:
%%% http://openid.net/specs/openid-authentication-2_0.html#normalization_example
%%%
normalize_test_() ->
    [
     % A URI with a missing scheme is normalized to a http URI
     ?_assertMatch("http://example.com/", normalize("example.com"))

     % An empty path component is normalized to a slash
     ,?_assertMatch("http://example.com/", normalize("http://example.com"))

     % https URIs remain https URIs
     ,?_assertMatch("https://example.com/", normalize("https://example.com/"))

     % No trailing slash is added to non-empty path components
     ,?_assertMatch("http://example.com/user", normalize("http://example.com/user"))

     % Trailing slashes are preserved on non-empty path components
     ,?_assertMatch("http://example.com/user/", normalize("http://example.com/user/"))

     % Trailing slashes are preserved when the path is empty
     ,?_assertMatch("http://example.com/", normalize("http://example.com/"))

     % Normalized XRIs start with a global context symbol
     ,?_assertMatch("=example", normalize("=example"))

     % Normalized XRIs start with a global context symbol
     ,?_assertMatch("=example", normalize("xri://=example"))

     % Fragments should be removed
     ,?_assertMatch("http://openid.net/specs/openid-authentication-2_0.html",
                    normalize("http://openid.net/specs/openid-authentication-2_0.html#normalization_example"))
].

-endif.
