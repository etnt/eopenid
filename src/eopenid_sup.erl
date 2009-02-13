%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   :  3 Apr 2008 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Re-worked :  7 Feb 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.     :  Implementation of OpenID v1.1 
%%%-------------------------------------------------------------------
-module(eopenid_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link(_) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_) ->
    EopenIdSrv = {eopenid_srv,{eopenid_srv,start_link,[]},
                  permanent,2000,worker,[eopenid_srv]},

    {ok,{{one_for_all,0,1}, [EopenIdSrv]}}.

