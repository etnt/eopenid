%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   :  3 Apr 2008 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Re-worked :  7 Feb 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.     :  Implementation of OpenID v1.1 
%%%-------------------------------------------------------------------
-module(eopenid).


-export([start/0, start/1, stop/0]).


start() ->
    application:start(inets),
    application:start(crypto),
    application:start(eopenid).

start(Type) ->
    application:start(inets),
    application:start(crypto),
    application:start(inets, Type).


stop() ->
    application:stop(eopenid).
