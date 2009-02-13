%%% -*- mode:erlang -*-
{application, eopenid,
  [{description, "Erlang consumer library for OpenID."},
   {vsn, "0.1.0"},
   {modules, [eopenid_app,eopenid_sup,eopenid_v1,eopenid_lib,eopenid_srv]},
   {registered, []},
   {applications, [kernel, stdlib, sasl, inets, crypto]}
  ]
}.

