-record(object, {name ::bookshelf_store:path(),
                 date :: calendar:datetime1970(),
                 size :: non_neg_integer(),
                 digest :: binary()}).

-record(bucket, {name::binary(),
                 date::calendar:datetime1970()}).
