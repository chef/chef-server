The goal of this document is to provide high-level overviews of how various systems work inside Chef Server. If you encounter a process
or feature that isn’t straightforward or took you time to understand consider writing an explanation here.

## Search Indexing

Chef Server performs searches by querying a search index, which is updated on every write to the Chef Server. That means every time we write to **oc_erchef's** postgresql database to update an object (such as a node, data bag, role, and so on), we also need to write to the search index.

This section describes how data gets from **erchef** to the search index. For how we query this data later, see Search Queries.

Chef Server 14 uses **Elasticsearch** as its search index. Previous versions of Chef Server supported both **Elasticsearch** and **Solr**.

Chef Server 14 has 2 different "search_queue_mode"s:

- batch
- inline

This configurable controls how the chef_index module sends requests to the search index. The default is `batch`.

Previous versions of Chef Server used a RabbitMQ-based search pipeline.

### Batch

```
  +--------+    +-------+
  | erchef | -> | Index |
  +--------+    +-------+

  Inside Erchef:

  +-------------------+    +------------------+
  | webmachine request|    | chef_index_batch |
  | handler process   | -> |     process      |
  +-------------------+    +------------------+
            ^                       |
            |              +--------V---------+
            |------------- | sender proc per  | -> HTTP request to
                           | batch            |    search index
                           +------------------+
```

Documents are directed from **erchef** to **ElasticSearch** in **batch** mode. Inside **erchef**, it works as follows:

1. Documents are expanded by the process handling the request (via code in chef_index_expand).

2. The expanded document is then sent to chef_index_batch.

3. chef_index_batch attempts to combine the document with other in-flight requests. Once a batch hits, the configurable max size or hits the configurable timeout, the batch is handed off to a newly spawned process that sends the batch to the index.

4. The webmachine request handling process blocks (it made a gen_server:call to chef_index_batch that we still haven't responded to until the call to the search index is complete. The sender process we spawned in (3) requests via the HTTP connection pool and then sends the response to all waiting webmachine request handler processes whose requests are in their batch.

### Inline

```
  +--------+    +-------+
  | erchef | -> | Index |
  +--------+    +-------+

  Inside Erchef:

  +-------------------+
  | webmachine request|    HTTP request to
  | handler process   | -> search index
  +-------------------+
```

Documents are directed from **erchef** to **ElasticSearch** in the `inline` mode. This mode can be useful for debugging. It is fairly
straightforward:

1. Documents are expanded by the process handling the request (via code in chef_index_expand).

2. The process handling the request makes a request to search index (via the search index HTTP connection pool) and waits for it to return.

Unlike **batch**, each write request to **erchef** generates an immediate inline request to the search index.

### Rabbitmq

```
  +--------+    +----------+    +------------------+    +------+
  | erchef | -> | rabbitmq | <- | opscode-expander | -> | Solr |
  +--------+    +----------+    +------------------+    +------+
```

Previous versions of Chef Server supported a **RabbitMQ** based indexing pipeline.  This is removed in Chef Server 14.

In the **rabbitmq search_queue_mode**, *erchef* places the object to be indexed on a *rabbitmq* queue. It moves forward with the request and returns a response to the user.  Another service named `opscode-expander` reads the object from the queue, expands it (see Document Expansion for details), and posts it to solr. 

Note that a failure to write to the search index does not trigger a failure of the API request that wrote the data since it happens asynchronously via a queue. However, a failure to place the item on *rabbitmq* causes a `500` error.

### Document Expansion

Before sending a document (such as a node object) to the search index, we reformat it using either **opscode-expander** or the code in erchef's **chef_index_expand** module.

We do this "expansion" to make better use of the search index.  For some historical details on this design, see: https://blog.chef.io/2012/01/20/post-hoc-index-design-from-regex-to-peg/.

The result of the expansion is that the JSON body of the object is flattened into a single field in the document that we had posted to the search index.  This field is structured so that we can later search against it to produce the illusion of having many separate fields.

An example: We have a node object with a body like:

```
  {
    "attr1" : {
        "attr2": "foo"
    },
    "attr3": "bar"
  }
```

We will post a document to the search index that looks something like:

```
{"content":"attr1_attr2__=__foo attr2__=__foo attr3__=__bar"}
```

Namely:

- All data is placed into the "content" field

- Nested keys are joined with `_`.

- Values are separated from keys with `__=__`

- Key-value pairs are separated from each other with spaces

- "Leaf" attributes are also indexed without their leading key elements to make searching for deeply nested values easier. (note the `attr2__=__foo` and the `attr1_attr2__=__foo`). One consequence is that top-level attributes and **leaf** attributes are indistinguishable from the search index.  This means that a search for `role:foo` might return more than the user expected values if a leaf attribute is also named `role`.

In addition to the raw data in the object, we add fields:

- X_CHEF_database_CHEF_X
- X_CHEF_id_CHEF_X
- X_CHEF_type_CHEF_X

So that we can construct searches for documents related to particular object types and organizations.

## Search Queries

```
  +------------+ 5   +------------+ 3  +------------+
  |chef-client | <-> |   erchef   | -> |search index|
  +------------+ 1   +------------+ 2  +------------+
                         | 4
                         v
                     +------------+
                     | postgresql |
                     +------------+
```

1. The client sends a search request via the search API: /search/node?q=*:*&start=0&rows=2

2. Erchef translates this into a search query to the search index. The **lucene** query is modified to account for the object expansion scheme we use. (Described in Document Expansion) Currently, we support two different backing stores for the search index: **Solr** and **ElasticSearch**. Importantly, for both *Solr* and *ElasticSearch*, the pagination is controlled entirely by the index backing store. The start and rows parameters are sent on from the user to the backing store.

3. The search index returns a list of IDs of documents that match the search. The search index does *NOT* return data.

4. Erchef queries the database for each object returned by the search. Any IDs that do not exist in the database are ignored. If `strict_search_result_acls` is enabled, results that the requesting user does not have permission to *READ* are also filtered from the results set.

5. Erchef responds to the API request either (normal search) with the full document or (partial search via a POST) with a reduced version of the document.

## FIPS Integration

This assumes you understand what the **FIPS 140-2** validation is. Putting the Chef Server into *FIPS mode* means:

1. It sets `OPENSSL_FIPS=1` in the environment, so shelling out to `openssl` activates the FIPS module.
2. Using the **erlang crypto** app it activates the FIPS module for any native calls.

The server can be switched into and out of FIPS mode at runtime. Edit the `chef-server.rb` config by adding `fips true` or `fips false` to force FIP mode as necessary. On systems where FIPS is enabled at the kernel level this config is defaulted to `true`. On all other systems it is defaulted to `false`. FIPS mode is supported on **RHEL** systems.

### FIPS Implementation Details

The erlang crypto app provides `crypto` module implementation. We no longer use the *erlang-crypto2* app.

- Setting `fips true` in `/etc/opscode/chef-server.rb` enables *FIPS* mode for Chef-Infra-Server.

## API v0 and v1 functionality for users

### v0

#### Expectation

- The user can edit all information, including their public\_key via v0 api (which knife is currently using).
- If the user performs an update that includes a nil public key, delete the default key from the keys table for that user.

#### Backend

- Since both the users and the keys table had the record of the public\_key there were some issues when the keys are deleted and recreated. Triggers updated the information into the keys table from the users table. (Could not be done with fk because both the user and client table reference the keys table).

#### Bug

- Since every v0 API command overwrites the public\_key in the users table, triggers, and copies the incorrect key onto the correct key in the keys table. This bug is fixed with `keys table is the only source if truth for public_key`.

#### Current Status

- Currently, phase1 is in rollout.
- knife uses v0.

### v1

- The user is not allowed to edit the public\_key via the /users endpoint, it must be managed via the /keys endpoint instead.
- chef-server-ctl uses v1.

## Keys table is the only source of truth for public\_key

### Phase1(Currently implemented)

- Add a function for add\_user and update\_user that inserts only the sentinel value into the public\_key field in the users table.
- The add and update triggers in keys\_update\_trigger.sql are present to maintain compatibility with older versions of chef-server.

### Phase2(To be implemented)

#### Todo

- Delete and update triggers in keys\_update\_trigger.sql.
- Delete the public\_key column in the users (and perhaps clients) table.


- This should be easier once all the users are on Phase1 release of chef-server.
- The code then interacts with the add\_user and update\_user functions in the back.
- The upgrade path after Phase2 rollout includes upgrading to a release with Phase1 first.

{{< /note >}}

## Buildkite

**Buildkite** is a platform for running fast, secure, and scalable continuous integration pipelines on your infrastructure. Pipelines contain unit, integration, and other tests to help assess and validate your build. The first step to investigating errors is to check the build logs.

The main pipelines for this repository are:

- `[chef/chef-server:main] verify`
- `[chef/chef-server:main] omnibus/adhoc`
- `[chef/umbrella:main] chef-server`

### [chef/chef-server:main] verify

**Verify** pipeline runs all the unit tests. A verify build is automatically triggered when changes to the branch are pushed, and a pull request is linked to it. The results of an automatically-triggered verify build are linked to the pull request. If the build fails the pull request is blocked. This build can also be triggered manually.

### [chef/chef-server:main] omnibus/adhoc

**omnibus/adhoc** pipeline runs the integration tests on different builds. Integration test covers the API endpoints of the project. The pipeline creates different builds for different supported OS/environments, and pushes the builds to *jfrog* artifactory. The integration tests scripts are tested against each of the different builds created. This pipeline is automatically triggered every night to make sure that the main is always ready to ship.

### [chef/umbrella:main] chef-server

**chef-server** pipeline is for end-to-end testing and creates different builds integrating with other projects of chef. These builds are packaged to replicate the different environments in which chef-server are used by the customers. This pipeline is run on a nightly basis using the latest build from the current omnibus pipeline.
