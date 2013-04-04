-record(object, {path :: binary(),
                 name :: binary(),
                 date :: calendar:datetime1970(),
                 size :: non_neg_integer(),
                 digest :: binary()}).

-record(bucket, {name:: string() | binary(),
                 date::calendar:datetime1970()}).
