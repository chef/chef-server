-record(chef_solr_query, {
          query_string :: string(),
          filter_query :: string(),
          start :: integer(),
          rows :: integer(),
          sort :: string(),
          index :: 'node'
                 | 'role'
                 | 'client'
                 | 'environment'
                 | {'data_bag', binary()}}).
