-record(object, {path :: binary() | undefined,
                 name :: binary() | undefined,
                 date :: calendar:datetime1970() | undefined,
                 size :: non_neg_integer() | undefined,
                 digest :: binary() | undefined}).

-record(bucket, {name:: string() | binary() | undefined,
                 date::calendar:datetime1970() | undefined}).
