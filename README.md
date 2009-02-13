This is an implementation of OpenID version 1.1.

(NB: IT IS NOT FINISHED YET AND STILL VERY ROUGH!)

Requirements: R12B-5 and Mochiweb

Start Erlang as:

    erl -pa ./ebin -pa ~/svn/mochiweb-read-only/ebin -s eopenid
    
At the moment you can try it out as shown below (nb: change the values
according to your setup):

    Dict0 = eopenid_lib:foldf(
              [in("openid.return_to", "http://www.tornkvist.org/openid"),
               in("openid.trust_root", "http://www.tornkvist.org")
              ], eopenid_lib:new()),
    {ok,Dict1} = eopenid_v1:discover("www.tornkvist.org", Dict0),
    {{ok, Url}, Dict2} = eopenid_v1:all(Dict1).


The above will perform DISCOVER, ASSOCIATE and return the Url to 
be used for CHECKID_SETUP.

Point a browser to the returned Url and login at the Provider.
Verify the Url you are "returned_to" as:

    eopenid_v1:verify_signed_keys(ReturnUrl, Dict)  =>  bool()

If you get 'true' returned, then you're authenticated.


TODO:

* Dumb mode.
* Server for holding the security association.
* Cleanup the code.





