RFC: Caching Search Results
===========================

Motivating Background
---------------------

A search query cache could significantly reduce request load on
couchdb and provide stop-gap overhead for OHC while work on
oc_erchef/SQL migration progresses.

As of January 2013, OHC load is pushing limits of couchdb. We are
starting to see a pattern of couchdb slow downs that lead to unicorn
backlogs and OHC API brown outs. These are resolved, so far, by
couchdb restart.

We used the following splunk query to estimate cache hits per minute
for repeated search queries that will pull data out of couch (all
non-node index queries):

    index=opscode-platform sourcetype=nginx-access
        (http_path="/organizations/*/search/*" AND NOT
         http_path="/organizations/*/search/node*")
    | timechart span=1m eval(c(http_path) - dc(http_path)) as dup_searches 


Looking at a 60 minute window at 20h58 17-01-2013 UTC showed
possible per minute cache hit rate between 724 and 1999. Since many
search requests involve multiple trips to couch due to batching of
bulk_get requests, we conclude that a search query cache could remove
significant load from couchdb especially during start of hour peak
load.

Cache Semantics and Invalidation
--------------------------------

We can identify repeated searches by keying on the API request with
query parameters. This includes the org name, index, query, and other
query modifiers.

The cache will store binary data resulting from a call to
`erlang:term_to_binary` of the following structure:

    {DbNumFound, Ans}

where the values of the tuple have the following meaning:

* `NumIds` is the count of the ids returned from the search
* `SolrNumFound` is the result count returned in solr response
   metadata
* `DbNumFound` is the number of found objects for the list of ids
* `Ans` is gzip of JSON search response.

This format will allow the oc_erchef search resource to return a
cached search result with minimal processing and log the request
metadata.

When processing a search request, the resource will always execute the
solr query. The cache will only be used if the solr query returns the
same set of ids and is younger than TTL seconds (propose 60 second TTL
as starting point).

The cache entries will use the following key format:

    :org_name-:request_digest

where `:org_name` is the name of the org and `:request_digest` is the
SHA1 digest in hex of the `term_to_binary` result of the following
list:

    [lists:sort(Ids), BatchSize, Start, ReqPath, Paths]

Where `Ids` are the ids returned from Solr, `BatchSize` is the
configured `bulk_get` batch size, `Start` is the pagination start
index, `ReqPath` is the full HTTP request path including query
parameters, and `Paths` is the parsed partial search path
specification.

Always asking solr for results means that we invalidate for the most
common search use cases where the main point is whether or not an item
is found.

A 60 second TTL for cache entries will mean that cached search results
will return a possibly out-of-date copy of objects. Note that we will
never return stale objects that fail to match the query (or at least
the caching will not change the behavior of the uncached system).

Without caching, deletes are immediate for search results. Even though
the solr index returns the deleted id, the object will not be found in
the db and will therefore not be included in search results. With
search result caching, deleted objects will appear in search results
until the next solr commit or search result expiration. Similarly,
without caching, search results always return the latest version of an
object (even if it is inconsistent with the search query). With
caching enabled, search results will return stale objects until the
search result cache entry expires.

Expected Size of Cache in RAM
-----------------------------

Splunk queries were used to estimate the expected cache size. To store
all unique search results over a 15 minute window will require
approximately 2GB. A 60 minute window requires 3.5GB.

Gzip'ing the content and one minute TTLs should reduce the size
required.

Queries used for this analysis:

    # total
    index=opscode-platform sourcetype=nginx-access host=lb-*
      eventtype="api_organizations_ORGNAME_search_INDEX"
      | chart eval(sum(body_bytes_sent) / 1024 / 1024) as body_mb_sent

    # total for unique http_paths
    index=opscode-platform sourcetype=nginx-access host=lb-*
      eventtype="api_organizations_ORGNAME_search_INDEX"
      | dedup http_path | chart eval(sum(body_bytes_sent) / 1024 / 1024) as u_body_mb_sent

    # example result
    last 15m: body_mb_sent = 7.7GB u_body_mb_sent = 1.6GB

Implementation
--------------

Use Redis as shared global cache data store. We don't need
persistence, but performance will be acceptable if redis does
I/O. Candidate locations for this redis instance:

* int-lb
* rabbitmq
* couch main
* couch authz

Add a `chef_search_cache` module to `chef_wm` that defines the cache
interface with no-op for OSC. Add a compile-time define
`?SEARCH_CACHE` that defaults to `chef_search_cache`. Call the macro
in the search resource.

In `oc_chef_wm` add a dependency on eredis. The eredis client is a
gen_server with async send/receive. Since exclusive access is not
needed, we could use pg2 to create a handful of clients to use. Write
the cache logic.


Why not Varnish?
----------------

Because couchdb HTTP calls go through bulk_get and there's no
identifying data that would allow us to distinguish search `bulk_gets`
from any other use. It also would not allow the invalidation via set
of ids returned from solr since there wouldn't be a place to encode
and store that data.
