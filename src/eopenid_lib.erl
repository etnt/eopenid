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
        ]).



http_get(Url) ->    
    http_get(Url, []).

http_get(Url, Hdrs) ->    
    http:request(get, {Url, Hdrs}, [], []).

http_post(Url, Hdrs, ContentType, Body) ->
    http:request(post, {Url,Hdrs,ContentType,Body}, [], []).
    

%%%
%%% Parse an nomalize the OpenID path.
%%% FIXME: Should be done according to ch.6 in RFC 3986.
%%%
http_path_norm("http://"++_ = Path) -> end_slash(Path);
http_path_norm(Path)                -> "http://"++end_slash(Path).

end_slash(Path) ->
    case lists:reverse(Path) of
        "/"++_ -> Path;
        Rev    -> lists:reverse([$/|Rev])
    end.
