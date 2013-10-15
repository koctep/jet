-define(common_fields, to, from, type).

-record(iq, {?common_fields, id, q, remove}).
-record(presence, {?common_fields, status}).
