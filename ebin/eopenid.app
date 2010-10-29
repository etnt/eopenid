%%% -*- mode:erlang -*-
{application, eopenid,
  [{description, "Erlang consumer library for OpenID."},
   {vsn, "0.1.0"},
   {mod, {eopenid_app, []}},
   {modules, [eopenid,eopenid_app,eopenid_sup,eopenid_v1,
              eopenid_v2,eopenid_lib,eopenid_srv]},
   {applications, [kernel, stdlib, inets, crypto]}
  ]
}.

