-ifndef(_EOPENID_DEBUG).
-define(_EOPENID_DEBUG, true).

-ifdef(debug).
-define(edbg(Fmt,Args), 
        error_logger:format("~p(~p): "++Fmt, [?MODULE,?LINE|Args])).
-else.
-define(edbg(Fmt,Args), true).
-endif.

-endif.
