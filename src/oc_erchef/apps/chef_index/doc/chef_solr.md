

# Module chef_solr #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Helper module for calling various Chef REST endpoints.
__Authors:__ John Keiser ([`jkeiser@chef.io`](mailto:jkeiser@chef.io)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_org_guid_to_query-2">add_org_guid_to_query/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_search_db-1">delete_search_db/1</a></td><td>Delete all search index entries for a given organization.</td></tr><tr><td valign="top"><a href="#delete_search_db_by_type-2">delete_search_db_by_type/2</a></td><td>Delete all search index entries for a given
organization and type.</td></tr><tr><td valign="top"><a href="#make_query_from_params-4">make_query_from_params/4</a></td><td></td></tr><tr><td valign="top"><a href="#ping-0">ping/0</a></td><td></td></tr><tr><td valign="top"><a href="#search-1">search/1</a></td><td></td></tr><tr><td valign="top"><a href="#solr_commit-0">solr_commit/0</a></td><td>Sends a "commit" message directly to Solr
This is exposed for the users of delete_search_db_by_type.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_org_guid_to_query-2"></a>

### add_org_guid_to_query/2 ###


<pre><code>
add_org_guid_to_query(Chef_solr_query::#chef_solr_query{}, OrgGuid::binary()) -&gt; #chef_solr_query{}
</code></pre>

<br></br>



<a name="delete_search_db-1"></a>

### delete_search_db/1 ###


<pre><code>
delete_search_db(OrgId::binary()) -&gt; ok
</code></pre>

<br></br>


Delete all search index entries for a given organization.
<a name="delete_search_db_by_type-2"></a>

### delete_search_db_by_type/2 ###


<pre><code>
delete_search_db_by_type(OrgId::binary(), Type::atom()) -&gt; ok
</code></pre>

<br></br>


Delete all search index entries for a given
organization and type.  Types are generally binaries or strings elsewhere in this
module. We should think about converting the other APIs in this file to use atoms
instead.
Note: This omits solr_commit because of the high cost of that call in production.
Some users will want to call the commit directly.
<a name="make_query_from_params-4"></a>

### make_query_from_params/4 ###


<pre><code>
make_query_from_params(ObjType::binary() | string(), QueryString::string() | binary() | undefined, Start::string(), Rows::string()) -&gt; #chef_solr_query{}
</code></pre>

<br></br>



<a name="ping-0"></a>

### ping/0 ###


<pre><code>
ping() -&gt; pong | pang
</code></pre>

<br></br>



<a name="search-1"></a>

### search/1 ###


<pre><code>
search(Chef_solr_query::#chef_solr_query{}) -&gt; {ok, non_neg_integer(), non_neg_integer(), [binary()]} | {error, {solr_400, string()}} | {error, {solr_500, string()}}
</code></pre>

<br></br>



<a name="solr_commit-0"></a>

### solr_commit/0 ###


<pre><code>
solr_commit() -&gt; ok | {error, term()}
</code></pre>

<br></br>


Sends a "commit" message directly to Solr
This is exposed for the users of delete_search_db_by_type
