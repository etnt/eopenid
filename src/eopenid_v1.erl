%%%-------------------------------------------------------------------
%%% Created   :  3 Apr 2008 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Re-worked :  7 Feb 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.     :  Implementation of OpenID v1.1 
%%%-------------------------------------------------------------------
-module(eopenid_v1).

-export([discover/1
        ]).

-ifdef(TEST).
-export([t/1
        ]).
-endif.

-import(eopenid_lib, 
        [new/0
         ,in/2
         ,in/3
         ,foldf/2
         ,http_get/1
         ,http_path_norm/1
         ,b2l/1
        ]).

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

-define(elog(Fmt,Args), 
        error_logger:format("~p(~p): "++Fmt, [?MODULE,?LINE|Args])).




%%% --------------------------------------------------------------------
%%% @spec discover(ClaimedId :: list()) -> {ok,dict()} | {Type,Error}.
%%%
%%% @doc Performs an OpenID discovery to find out the the 
%%%      provider and the (optional) delegated ID.
%%%      Returns an orddict() containing the discovred parameters.
%%% @end
%%% --------------------------------------------------------------------
-type( dict() :: list() ).
-type( etype() :: error | exit | throw ).
-type( error() :: any() ).

-spec discover( string() ) -> {ok,dict()} | {etype(),error()}.
    
discover(ClaimedId) ->
    NormId = http_path_norm(ClaimedId),
    {ok, {_Rc, _Hdrs, Body}} = http_get(NormId),
    try
        {Xml,_} = xmerl_scan:string(Body),
        L = [?XAttrs(X) || X <- xmerl_xpath:string("//link", Xml)],
        Fs = [in(K,V) || [{rel,K},{href,V}] <- L],
        {ok, foldf(Fs, new())}
    catch
        _Type:_Error ->
            ?elog("discover xmerl failed~n", []),
            try 
                A = mochiweb_html:parse(Body),
                %%?elog("A=~p~n",[A]),
                {ok, hvals(gelems([<<"html">>,<<"head">>,<<"link">>], A))}
            catch
                Type2:Error2 ->
                    %% FIXME try doing the parsing in some other way
                    ?elog("discover mochiweb failed: ~p, Body=~p~n",
                          [erlang:get_stacktrace(),Body]),
                    {Type2, Error2}
            end
    end.


-spec hvals( list() ) -> dict().

%%% Header values
hvals(Vs) ->
    hvals(new(), Vs).

hvals(S, [{<<"link">>,[{<<"rel">>,R},{<<"href">>,H}],_} | T]) -> 
    hvals(pick(S,R,H), T);
hvals(S, [_|T]) -> 
    hvals(S, T);
hvals(S, []) -> 
    S.

pick(S, <<"openid.server">>=K, V)   -> in(b2l(K), b2l(V), S);
pick(S, <<"openid.delegate">>=K, V) -> in(b2l(K), b2l(V), S);
pick(S, _, _)                       -> S.
    

-type( tree() :: tuple() ).
-type( set() :: list() ).

-spec gelems( [binary()] , [tree()] ) -> set().

gelems(Path, Tree) ->
    ordsets:to_list(gelems(Path, [Tree], ordsets:new())).

gelems([E], L, S)             -> 
    lists:foldl(fun({X,_,_}=N, Acc) when X == E ->
                        ordsets:add_element(N,Acc);
                   (_, Acc) -> Acc
                end, S, L);
gelems([E|T], L, S) when is_list(L) ->
    lists:foldl(fun({X,_,Xl}, Acc) when X == E ->
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
