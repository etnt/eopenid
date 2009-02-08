%%%-------------------------------------------------------------------
%%% Created   :  3 Apr 2008 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Re-worked :  7 Feb 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.     :  Implementation of OpenID v1.1 
%%%-------------------------------------------------------------------
-module(eopenid_lib).

-export([http_get/1
         ,http_get/2
         ,http_post/4
         ,http_path_norm/1
         ,new/0
         ,out/2
         ,in/2
         ,in/3
         ,foldf/2
         ,add/3
         ,b2l/1
        ]).


%%%
%%% DICT API
%%%
new() ->
    orddict:new(). 

%%% Should crash if no value is found
out(Key, DB) ->
    {ok, Value} = orddict:find(Key, DB),
    Value.

%%% 
in(Key, Value) ->
    fun(Data) -> in(Key, Value, Data) end.

in(Key, Value, DB) ->
    orddict:store(Key, Value, DB).

%%% 
foldf([], DB)    -> DB;
foldf([H|T], DB) -> foldf(T, H(DB)).

%%% Add value to a list of values.
add(Key, Value, Data) ->
    try out(Key, Data) of
        L when is_list(L) -> in(Key, [Value|L], Data)
    catch
        _:_ -> in(Key, [Value], Data)
    end.                                              
            

%%%
%%%@doc Perform an HTTP GET request.
%%%@end
http_get(Url) ->    
    http_get(Url, []).

http_get(Url, Hdrs) ->    
    http:request(get, {Url, Hdrs}, [], []).

%%% 
%%%@doc Perform an HTTP POST request.
%%%@end
http_post(Url, Hdrs, ContentType, Body) ->
    http:request(post, {Url,Hdrs,ContentType,Body}, [], []).
    

%%%
%%%@doc Parse an nomalize the OpenID path.
%%%     FIXME: Should be done according to ch.6 in RFC 3986.
%%%@end
http_path_norm("http://"++_ = Path) -> end_slash(Path);
http_path_norm(Path)                -> "http://"++end_slash(Path).

end_slash(Path) ->
    case lists:reverse(Path) of
        "/"++_ -> Path;
        Rev    -> lists:reverse([$/|Rev])
    end.


b2l(B) when is_binary(B) -> binary_to_list(B);
b2l(L) when is_list(L)   -> L.

    
