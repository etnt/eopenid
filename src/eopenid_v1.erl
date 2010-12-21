%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   :  3 Apr 2008 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Re-worked :  7 Feb 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.     :  Implementation of OpenID v1.1 
%%%
%%%-------------------------------------------------------------------
-module(eopenid_v1).

-export([discover/1
         ,discover/2
	 ,discover/3
         ,associate/1
         ,checkid_setup/1
         ,verify_signed_keys/2
        ]).

-ignore_xref([{discover,1}]).

-ifdef(TEST).
-export([t/1
         ,all/0
         ,all/1
        ]).
-endif.

-import(eopenid_lib, 
        [new/0
         ,in/2
         ,in/3
         ,out/2
         ,foldf/2
         ,http_get/1
         ,http_post/4
         ,http_path_norm/1
         ,b2l/1
         ,i2l/1
         ,urlenc/1
         ,parseq/1
         ,content_type/0
         ,implode/2
         ,roll/1
         ,unroll/1
         ,compute_K/1
         ,compute_B/1
         ,decrypt_mac_key/1
        ]).

-include("eopenid_typespecs.hrl").
-include("eopenid_debug.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(xelem(N,As,C), #xmlElement{name       = N, 
                                   attributes = As,
                                   content    = C}).

-define(xattr(N,V), #xmlAttribute{name  = N, 
                                  value = V}).

-define(xtxt(T), #xmlText{value = T}).

-define(XAttrs(X),
    (fun() ->
             ?xelem(_,As,_) = X,
             [{N,V} || ?xattr(N,V) <- As] end)()).


%%%
%%% --- TEST ---
%%%
%%% Does DISCOVER, ASSOCIATE and returns the Url to be used for CHECKID_SETUP.
%%% Returns: {Url, Dict}
%%%
%%% Point a browser to the returned Url and login at the Provider.
%%% Verify the Url you are "returned_to" as:
%%%
%%%  verify_signed_keys(ReturnUrl, Dict)
%%%
all() ->
    Dict0 = foldf([in("openid.return_to", "http://www.tornkvist.org/openid"),
                   in("openid.trust_root", "http://www.tornkvist.org")
                  ], new()),
    {ok,Dict1} = discover("www.tornkvist.org", Dict0),
    all(Dict1).

all(Dict) ->
    {ok,Dict1} = associate(Dict),
    {checkid_setup(Dict1), Dict1}.


%%% --------------------------------------------------------------------
%%% @spec verify_signed_keys( string() , dict() ) -> bool().
%%%
%%% @doc Ask the IP if an end user owns the Claimed ID.
%%%      Returns an URL that the end user should be redirected to.
%%% @end
%%% --------------------------------------------------------------------
-spec verify_signed_keys( string(), dict() ) -> bool().
    
verify_signed_keys(Url, Dict0) -> 
    Qargs = get_query_args(Url),
    D = orddict:from_list(parseq(Qargs)),
    Dict = orddict:merge(fun(_,V,_) -> V end, D, Dict0),
    Signed = out("openid.signed", Dict),
    Keys = string:tokens(Signed, ","),
    L = [Key++":"++out("openid."++Key,Dict)++"\n" || Key <- Keys],
    TokenContents = lists:flatten(L),
    MacKey = out("openid.mac_key", Dict),
    MySig  = crypto:sha_mac(MacKey, TokenContents),
    SrvSig = base64:decode(out("openid.sig",Dict)),
    try 
        true == (MySig =:= SrvSig)
    catch _:_ -> 
            ?edbg("verify_signed_keys: MySig=~p , SrvSig=~p~n", [MySig,SrvSig]),
            false
    end.

get_query_args(Url) ->
    case string:tokens(Url, "?") of
        [Qargs]   -> Qargs;
        [_,Qargs] -> Qargs
    end.
            

%%% --------------------------------------------------------------------
%%% @spec checkid_setup( dict() ) -> {ok,string()} | {Type,Error}.
%%%
%%% @doc Ask the IP if an end user owns the Claimed ID.
%%%      Returns an URL that the end user should be redirected to.
%%% @end
%%% --------------------------------------------------------------------
-spec checkid_setup( dict() ) -> {ok,string()} | {etype(),error()}.
    
checkid_setup(Dict) ->
    ?edbg("+++ checkid_setup, Dict=~p~n",[Dict]),
    Mode         = "checkid_setup",
    Identity     = claimed_id(Dict),
    AssocHandle  = out("openid.assoc_handle", Dict),
    ReturnTo     = out("openid.return_to", Dict),
    TrustRoot    = out("openid.trust_root", Dict),
    Provider     = out("openid.server", Dict),

    Keys = ["mode","identity","assoc_handle","return_to","trust_root"],
    L = lists:zip(["openid."++K || K <- Keys],
                  [Mode,Identity,AssocHandle,ReturnTo,TrustRoot]),
    {ok, Provider++"?"++urlenc(L)}.

claimed_id(Dict) ->
    try out("openid.delegate", Dict)
    catch _:_ -> out("openid.claimed_id", Dict)
    end.
            


%%% --------------------------------------------------------------------
%%% @spec associate( dict() ) -> {ok,dict()} | {Type,Error}.
%%%
%%% @doc Performs an OpenID association with the server in the Dict.
%%%      Returns an orddict() containing the assoc. parameters.
%%% @end
%%% --------------------------------------------------------------------
-spec associate( dict() ) -> {ok,dict()} | {etype(),error()}.
    
associate(Dict) ->
    try 
        Provider = out("openid.server", Dict),
        {ok,Adict} = eopenid_srv:get_assoc_dict(Provider),
        ?edbg("+++ associate, Got Adict=~p~n",[Adict]),
        {ok, in("openid.assoc_handle", out("assoc_handle", Adict), Dict)}
    catch
        _:_ -> do_associate(Dict)
    end.

do_associate(Dict) ->
    Mode             = "associate",
    AssocType        = "HMAC-SHA1",
    SessionType      = "DH-SHA1",
    {A,DHa,G,P}      = eopenid_lib:mk_dh(),
    DH_modulus       = base64:encode_to_string(roll(P)),
    DH_gen           = base64:encode_to_string(roll(G)),
    DH_consumer_pub  = base64:encode_to_string(roll(A)),
    Provider         = out("openid.server", Dict),

    %% Create the keys to be sent in the request
    L = lists:zip(["openid."++K || K <- assoc_keys()],
                  [AssocType,SessionType,DH_modulus,DH_gen,DH_consumer_pub]),

    %% Create the Association Handle dict, to be stored for future use.
    Adict = lists:foldl(fun({K,V},D) -> in(K,V,D) end, 
                        foldf([in("a",DHa),
                               in("A",A),
                               in("p",P),
                               in("g",G)
                               ], new()), 
                        L),

    Body = urlenc([{"openid.mode",Mode}|L]),
    associate_request(Adict, Body, Dict, Provider).

associate_request(Adict, Body, Dict, Provider) ->
    Hdrs = [],
    ContentType = content_type(), 
    case http_post(Provider, Hdrs, ContentType, Body) of
        {ok, {{_,200,_}, _Rhdrs, Rbody}} ->
            Q = [tokenize(KV,$:) || KV <- string:tokens(Rbody, "\n")],
            ?edbg("+++ Rhdrs=~p , Repl-Body=~p, Q=~p~n",[_Rhdrs,Rbody,Q]),
            Adict1 = lists:foldl(fun({K,V},D) -> 
                                         in(K,V,D) 
                                 end, 
                                 Adict, Q),
            Adict2 = decrypt_mac_key(compute_K(compute_B(Adict1))),
            eopenid_srv:put_assoc_dict(Provider, Adict2),
            {ok,in("openid.assoc_handle", 
                   out("assoc_handle", Adict2), 
                   in("openid.mac_key", 
                      out("mac_key", Adict2), 
                      Dict))};

        Else ->
            ?edbg("+++ http_post not ok: ~p~n",[Else]),
            {error, Else}
    end.


tokenize(String, Separator) ->
    tokenize(String, Separator, []).
    
tokenize([H|T], H, Acc) -> {lists:reverse(Acc), T};
tokenize([H|T], C, Acc) -> tokenize(T, C, [H|Acc]);
tokenize([], _, Acc)    -> {lists:reverse(Acc), ""}.

assoc_keys() ->
    ["assoc_type", "session_type", "dh_modulus",
     "dh_gen", "dh_consumer_public"].
    

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

discover(ClaimedId, Dict) ->
    discover(ClaimedId, Dict, all).

discover(ClaimedId, Dict0, Parser) when is_list(ClaimedId) ->
    NormId = http_path_norm(ClaimedId),
    Dict = in("openid.claimed_id", ClaimedId, Dict0),
    {ok, {_Rc, _Hdrs, Body}} = http_get(NormId),
    parse_body(Body, Dict, Parser).

parse_body(Body, Dict, all) ->
    try
	xmerl_parse_body(Body, Dict)
    catch
        _Type:_Error ->
            try
		trane_parse_body(Body, Dict)
            catch
                _:_ ->
                    try
			mochiweb_parse_body(Body, Dict)
                    catch
                        _Type2:_Error2 ->
                            try
				yaws_parse_body(Body, Dict)
                            catch
                                Type3:Error3 ->
                                    %% FIXME try doing the parsing in some other way
                                    ?edbg("discover failed: ~p, Body=~p~n",
                                          [erlang:get_stacktrace(),Body]),
                                    {Type3, Error3}
                            end
                    end
            end
    end;
parse_body(Body, Dict, mochiweb) ->
    try
	mochiweb_parse_body(Body, Dict)
    catch
	Type3:Error3 ->
	    ?edbg("discover failed: ~p, Body=~p~n",
		  [erlang:get_stacktrace(),Body]),
	    {Type3, Error3}
    end;
parse_body(Body, Dict, yaws) ->
    try
	yaws_parse_body(Body, Dict)
    catch
	Type3:Error3 ->
	    ?edbg("discover failed: ~p, Body=~p~n",
		  [erlang:get_stacktrace(),Body]),
	    {Type3, Error3}
    end;
parse_body(Body, Dict, trane) ->
    try
	trane_parse_body(Body, Dict)
    catch
	Type3:Error3 ->
	    ?edbg("discover failed: ~p, Body=~p~n",
		  [erlang:get_stacktrace(),Body]),
	    {Type3, Error3}
    end;
parse_body(Body, Dict, xmerl) ->
    try
	xmerl_parse_body(Body, Dict)
    catch
	Type3:Error3 ->
	    ?edbg("discover failed: ~p, Body=~p~n",
		  [erlang:get_stacktrace(),Body]),
	    {Type3, Error3}
    end.

xmerl_parse_body(Body, Dict) ->
    {Xml,_} = xmerl_scan:string(Body),
    L = [?XAttrs(X) || X <- xmerl_xpath:string("//link", Xml)],
    Fs = [in(K,V) || [{rel,K},{href,V}] <- L],
    {ok, foldf(Fs, Dict)}.

trane_parse_body(Body, Dict) ->
    Fs2 = [in(K,V) || {tag,"link",[{"rel","openid"++_=K},{"href",V}|_]}
			  <- trane:sax(Body,fun(T,A)-> A++[T] end, [])],
    {ok, foldf(Fs2, Dict)}.

mochiweb_parse_body(Body, Dict) ->
    A = mochiweb_html:parse(Body),
    {ok, hvals(Dict, gelems([<<"html">>,<<"head">>,<<"link">>], A))}.

yaws_parse_body(Body, Dict) ->
    Y = yaws_html:h2e(Body),
    {ok, hvals(Dict, gelems([ehtml,html,head,link], Y))}.

%%% Header values
-spec hvals( dict(), list() ) -> dict().

hvals(S, [{<<"link">>,[{<<"rel">>,R},{<<"href">>,H}],_} | T]) -> % Mochiweb
    hvals(pick(S,R,H), T);
hvals(S, [{link,[{rel,R},{href,H}|_]} | T]) -> % Yaws
    hvals(pick(S,R,H), T);
hvals(S, [_|T]) -> 
    hvals(S, T);
hvals(S, []) -> 
    S.

pick(S, <<"openid.server">>=K, V)   -> in(b2l(K), b2l(V), S);
pick(S, <<"openid.delegate">>=K, V) -> in(b2l(K), b2l(V), S);
pick(S, "openid.server"=K, V)       -> in(K, V, S);
pick(S, "openid.delegate"=K, V)     -> in(K, V, S);
pick(S, _, _)                       -> S.
    

-type( tree() :: tuple() ).
-type( set() :: list() ).

-spec gelems( [binary()] , [tree()] ) -> set().

gelems(Path, Tree) ->
    ordsets:to_list(gelems(Path, [Tree], ordsets:new())).

gelem_add(E, {X,_,_}=N, Acc) when X == E ->
    ordsets:add_element(N,Acc);
gelem_add(E, {X,_}=N, Acc) when X == E ->
    ordsets:add_element(N,Acc);
gelem_add(E, _, Acc) -> Acc.

gelems([E], L, S) when is_list(L) -> 
    F = fun(N, Acc) -> gelem_add(E, N, Acc) end,
    lists:foldl(F, S, L);
gelems([E], I, S) ->
    gelem_add(E, I, S);
gelems([E|T], L, S) when is_list(L) ->
    lists:foldl(fun({X,_,Xl}, Acc) when X == E ->
                        gelems(T, Xl, Acc);
                   ({X,Xl}, Acc) when X == E ->
                        gelems(T, Xl, Acc);
                   (_, Acc) -> Acc
                end, S, L);
gelems(_, _, S) -> S.




-ifdef(EUNIT).






gelem_test() ->
    ?assertMatch([{<<"link">>,
                   [{<<"rel">>,<<"openid.delegate">>},
                    {<<"href">>,<<"http://etnt.myopenid.com/">>}],
                   []},
                  {<<"link">>,
                   [{<<"rel">>,<<"openid.server">>},
                    {<<"href">>,<<"http://www.myopenid.com/server">>}],
                   []}],
                 t(1)).


t(1) ->
    gelems([<<"html">>,<<"head">>,<<"link">>], data()).


data() ->
    {<<"html">>,[],
     [{<<"head">>,[],
       [{<<"title">>,[],[<<"www.tornkvist.org">>]},
        {<<"link">>,
         [{<<"rel">>,<<"openid.server">>},
          {<<"href">>,<<"http://www.myopenid.com/server">>}],
         []},
        {<<"link">>,
         [{<<"rel">>,<<"openid.delegate">>},
          {<<"href">>,<<"http://etnt.myopenid.com/">>}],
         []}]},
      {<<"body">>,[],
       [{<<"p">>,[],
         [<<"\n      There aren't much here, but have a look at the links below.\n    ">>,
          {<<"p">>,[],
           [{<<"p">>,[],
             [{<<"a">>,
               [{<<"href">>,<<"http://blog.tornkvist.org/">>}],
               [<<"The Red Hot Erlang Blog">>]}]},
            {<<"p">>,[],
             [{<<"a">>,
               [{<<"href">>,
                 <<"http://www.tornkvist.org/gitweb/">>}],
               [<<"My Git Repository">>]}]},
            {<<"p">>,[],
             [{<<"a">>,
               [{<<"href">>,<<"http://tpl.tornkvist.org/">>}],
               [<<"The Priority List">>]}]},
            {<<"p">>,[],
             [{<<"a">>,
               [{<<"href">>,<<"http://domerl.tornkvist.org/">>}],
               [<<"A Domerl Example">>]}]}]}]}]}]}.


-endif.
