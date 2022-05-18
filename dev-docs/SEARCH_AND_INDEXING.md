## Search Indexing

Chef Server indexes all objects (nodes, data bags, cookbooks, roles, etc) in a search engine. Whenever an object is updated in Chef Server, the updated version is sent to the search provider to be indexed (or deleted).

Chef Server 15 makes use of OpenSearch as its default search provider. External OpenSearch and ElasticSearch are also supported as providers in 15. Chef Server 14 shipped OpenSearch as its default provider.  Earlier versions supported and shipped Solr.

Chef Server performs searches on demand by querying the search provider against the appropriate index.  A separate index for each data type is used (node, cookbooks, roles, etc); and each data bag gets its own index.

This section describes how data gets from **erchef** to the search provider. For how we query this data later, see Search Queries.

Chef Server 14 has 2 different "search_queue_mode"s:

- batch
- inline

This configurable controls how the `chef_index` module sends requests to the search provider. The default is `batch`.

Previous versions of Chef Server used a RabbitMQ-based search pipeline. See "Historical" section at the end of the document for more information.



### Batch

Batch is the default mode.  The net effect of `batch` mode is that multiple requests over a short period of time get submitted together for more efficient indexing, reducing load on/round trips to the indexing provider.

```
  +--------+    +-------+
  | erchef | -> | Index |
  +--------+    +-------+

  Inside Erchef:

1 +-------------------+    +------------------+
  | webmachine request| 2  | chef_index_batch |
  | handler process   | -> |     process      |
  +-------------------+    +------------------+
            ^                       |3
            |              +--------V---------+
            |------------- | sender proc per  | -> HTTP request to
                4          | batch            |    search provider
                           +------------------+
```

Documents are directed from **erchef** to the search provider in **batch** mode. Inside **erchef**, it works as follows:

1. Chef Infra objects (such as nodes, roles, et al) are 'expanded' to searchable documents via `chef_index_expand`. This is invoked by the webmachine process handling the object update request. See 'Document Expansion' later in this document for more details.

2. The expanded document is then sent to chef_index_batch.

3. chef_index_batch attempts to combine the document with other in-flight requests. Once a batch reaches the configurable max size (`search_batch_max_size`) or hits the configurable timeout (`search_batch_max_wait`), the batch is handed off to a newly spawned process that sends the batch to the index.

4. The webmachine request handling process blocks (it made a gen_server:call to chef_index_batch that we still haven't responded to) until the call to the search provider is complete. The sender process we spawned in (3) requests via the HTTP connection pool and then sends the response to all waiting webmachine request handler processes whose requests are in their batch.

### Inline

This mode can be used as a tool to troubleshoot search requests, or on low-volume servers.

```
  +--------+    +-----------------+
  | erchef | -> | Search Provider |
  +--------+    +-----------------+

  Inside Erchef:

  +-------------------+
  | webmachine request|    HTTP request to
  | handler process   | -> Search Provider (OpenSearch)
  +-------------------+
```

Documents are directed from **erchef** to **OpenSearch** in the `inline` mode.


1. The modified object is expanded by the webmachine process handling the request via `chef_index_expand`. See 'Document Expansion' later in this document for more details.

2. The process handling the request makes a request to search provider (via the search provider HTTP connection pool) and waits for it to return.

Unlike **batch**, each write request to **erchef** generates an immediate inline request to the search provider.

## Document Expansion

Before sending a document (such as a node object) to the search provider, we reformat it using code in erchef's **chef_index_expand** module.

We do this "expansion" to make better use of the search provider.  For some historical details on this design, see: https://www.chef.io/blog/post-hoc-index-design-from-regex-to-peg

The result of the expansion is that the JSON body of the object is flattened into a single field in the document that we will post to the search provider.  This field is structured so that we can later search against it to produce the illusion of having many separate fields.

An example: We have a node object with a body like:

```
  {
    "attr1" : {
        "attr2": "foo"
    },
    "attr3": "bar"
  }
```

We will post a document to the search provider that looks something like:

```
{"content":"attr1_attr2__=__foo attr2__=__foo attr3__=__bar"}
```

Namely:

- All data is placed into the "content" field
- Nested keys are joined with `_`.
- Values are separated from keys with `__=__`
- Key-value pairs are separated from each other with spaces
- "Leaf" attributes are also indexed without their leading key elements to make searching for deeply nested values easier. (note the `attr2__=__foo` and the `attr1_attr2__=__foo`). One consequence is that top-level attributes and **leaf** attributes are indistinguishable from the search provider.  This means that a search for `role:foo` might return more than the user expected values if a leaf attribute is also named `role`.

In addition to `content` which contains the the raw data in the object, we add fields:

- `X_CHEF_database_CHEF_X` - the id of the owning organization
- `X_CHEF_id_CHEF_X` - the internal id of the object
- `X_CHEF_type_CHEF_X` - object type (role, cookbook, node, etc)

This allows us to construct searches for documents related to particular object types and organizations.

## Search Queries

```
  +------------+ 5   +------------+ 3  +---------------+
  |chef-client | <-> |   erchef   | -> |search provider|
  +------------+ 1   +------------+ 2  +---------------+
                         | 4
                         v
                     +------------+
                     | postgresql |
                     +------------+
```

1. The client sends a search request via the search API: /search/node?q=*:*&start=0&rows=2

2. Erchef translates this into a search query to the search provider. The **lucene** query is modified to account for the object expansion scheme we use. (Described in Document Expansion.)

3. The search provider returns a list of IDs of documents that match the search. The search provider does *NOT* return data.

4. Erchef queries the database for each object returned by the search. Any IDs that do not exist in the database are ignored. If `strict_search_result_acls` is enabled, results that the requesting user does not have permission to *READ* are also filtered from the results set.

5. Erchef responds to the API request either (normal search) with the full document or (partial search via a POST) with a reduced version of the document.

## Historical Notes

### Rabbitmq

Rabbitmq/opscode-expander has not been included since Chef Server 14.  This is kept as historical reference.


```
  +--------+    +----------+    +------------------+    +------+
  | erchef | -> | rabbitmq | <- | opscode-expander | -> | Solr |
  +--------+    +----------+    +------------------+    +------+
```

Previous versions of Chef Server supported a **RabbitMQ** based indexing pipeline.  This is removed in Chef Server 14.

In the **rabbitmq search_queue_mode**, *erchef* places the object to be indexed on a *rabbitmq* queue. It moves forward with the request and returns a response to the user.  Another service named `opscode-expander` reads the object from the queue, expands it (see Document Expansion for details), and posts it to solr.

Note that a failure to write to the search provider does not trigger a failure of the API request that wrote the data since it happens asynchronously via a queue. However, a failure to place the item on *rabbitmq* causes a `500` error.
