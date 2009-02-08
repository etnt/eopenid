This is an implementation of OpenID version 1.1.

Requirements: R12B-5 and Mochiweb

To run the currently working parts:

    erl -pa ./ebin -pa ~/svn/mochiweb-read-only/ebin -s inets -s crypto
    
    eopenid_v1:discover("www.tornkvist.org").
    
    % To run the Unit Tests
    eopenid_v1:test().




