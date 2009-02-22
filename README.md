This is an implementation of OpenID version 1.1.

(NB: IT DOES NOT (YET) IMPLEMENT THE DUMB MODE!)

Requirements: R12B-5 and Mochiweb

Start Erlang as:

    erl -pa ./ebin -pa ~/svn/mochiweb-read-only/ebin -s eopenid
    
At the moment you can try it out as shown below (nb: change the values
according to your setup). Also, this example is using some Nitrogen
code (wf.erl):

    [ClaimedId] = wf:q(claimed_id),
    Dict0 = eopenid_lib:foldf(
              [eopenid_lib:in("openid.return_to", 
                              "http://www.tornkvist.org:8002/web/openid"),
               eopenid_lib:in("openid.trust_root", 
                              "http://www.tornkvist.org:8002")
              ], eopenid_lib:new()),
    {ok,Dict1} = eopenid_v1:discover(ClaimedId, Dict0),
    {ok,Dict2} = eopenid_v1:associate(Dict1),
    {ok, Url}  = eopenid_v1:checkid_setup(Dict2).
    wf:session(eopenid_dict, Dict2),
    wf:redirect(Url)

The above will perform DISCOVER, ASSOCIATE, CHECKID_SETUP and 
return the Url to be used for redirecting the client. 
Note that an associate request only is made the first time eopenid 
see a new Provider (the Association is cached the very first time).
The Nitrogen framework will in this example send a redirect to the 
client, who will login to its Provider and (again) will be redirected, 
this time to the *return_to* address.

At the *return_to* address we have the following code which
verifies the signed keys that the Provider sent along with the
redirect in the *returned_to* Url:

    Dict = wf:session(eopenid_dict),
    RawPath = wf_platform:get_raw_path(),
    %% assertion
    true = eopenid_v1:verify_signed_keys(RawPath, Dict),
    ClaimedId = eopenid_lib:out("openid.claimed_id", Dict),
    wf:user(ClaimedId),
    wf:redirect("/web/write/")

Finally, the authenticated user is redirected to the protected page.


TODO:

* Dumb mode.
* Cleanup the code.





