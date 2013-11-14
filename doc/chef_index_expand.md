

# Module chef_index_expand #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Chef object flatten/expand and POSTing to Solr.

<a name="description"></a>

## Description ##



This module implements Chef object flatten/expand using the same
algorithm as `chef-expander` and handles POSTing updates (both adds
and deletes) to Solr. You will interact with four functions:



1. [`init_items/1`](#init_items-1)

1. [`add_item/5`](#add_item-5)

1. [`delete_item/4`](#delete_item-4)

1. [`send_items/1`](#send_items-1)




Start by initializing an item context object using [`init_items/1`](#init_items-1) to which you pass the URL for the Solr instance you
want to work with.



Next, use [`add_item/5`](#add_item-5) to add/update items in the
index. These items will go through the flatten/expand process. If
you want to stage an item delete, use [`delete_item/4`](#delete_item-4). Both
of these functions take an item context object and return a
possibly updated context. It is important to keep track of the
possibly modified context to use for your next call. This API
allows this module to handle the flatten/expand and post in
different ways. For example, the flatten/expand can be done inline,
accumulating the result in the context, or the context can contain
a pid and the work can be done async and in parallel.


Finally, call [`send_items/1`](#send_items-1) passing the accumulated context
object. Calling this function triggers the actual POST to solr.

<a name="types"></a>

## Data Types ##




### <a name="type-index_expand_ctx">index_expand_ctx()</a> ###


__abstract datatype__: `index_expand_ctx()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_item-5">add_item/5</a></td><td>Add an EJSON item to the provided index expand context.</td></tr><tr><td valign="top"><a href="#delete_item-4">delete_item/4</a></td><td>Add <code>Id</code> to the list of items to delete from solr.</td></tr><tr><td valign="top"><a href="#init_items-1">init_items/1</a></td><td>Create a new index expand context.</td></tr><tr><td valign="top"><a href="#make_command-5">make_command/5</a></td><td>Create a "command" EJSON term given Chef object attributes
<code>Type</code>, <code>ID</code>, <code>DatabaseName</code>, and <code>Item</code>.</td></tr><tr><td valign="top"><a href="#post_multi-1">post_multi/1</a></td><td>Given a list of command EJSON terms, as returned by <a href="#make_command-4"><code>make_command/4</code></a>, perform the appropriate flatten/expand operation
and POST the result to Solr as a single update.</td></tr><tr><td valign="top"><a href="#post_single-1">post_single/1</a></td><td>Given a command EJSON term as returned by <a href="#make_command-4"><code>make_command/4</code></a>, flatten/expand and POST to Solr.</td></tr><tr><td valign="top"><a href="#send_items-1">send_items/1</a></td><td>Send items accumulated in the index expand context to
Solr.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_item-5"></a>

### add_item/5 ###


<pre><code>
add_item(Idx_exp_ctx::<a href="#type-index_expand_ctx">index_expand_ctx()</a>, Id::binary(), Ejson::<a href="ej.md#type-json_object">ej:json_object()</a>, Index::binary() | atom(), OrgId::binary()) -&gt; <a href="#type-index_expand_ctx">index_expand_ctx()</a>
</code></pre>

<br></br>


Add an EJSON item to the provided index expand context. The
backing implementation will flatten/expand `Ejson` either inline
(blocking) or async/parallel (in which case this function returns
immediately).
<a name="delete_item-4"></a>

### delete_item/4 ###


<pre><code>
delete_item(Idx_exp_ctx::<a href="#type-index_expand_ctx">index_expand_ctx()</a>, Id::binary(), Index::binary() | atom(), OrgId::binary()) -&gt; <a href="#type-index_expand_ctx">index_expand_ctx()</a>
</code></pre>

<br></br>


Add `Id` to the list of items to delete from solr.
<a name="init_items-1"></a>

### init_items/1 ###


<pre><code>
init_items(SolrUrl::string()) -&gt; <a href="#type-index_expand_ctx">index_expand_ctx()</a>
</code></pre>

<br></br>


Create a new index expand context.
<a name="make_command-5"></a>

### make_command/5 ###


<pre><code>
make_command(Action::add | delete, Type::binary() | atom(), ID::binary(), DatabaseName::binary() | string(), Item::term()) -&gt; term()
</code></pre>

<br></br>



Create a "command" EJSON term given Chef object attributes
`Type`, `ID`, `DatabaseName`, and `Item`. The returned EJSON has
the same form as we use to place on the RabbitMQ queue for indexing
and that chef-expander expects to find for processing. The
`DatabaseName` can be either an OrgId or "chef_1deadbeef". The
`Item` should be the EJSON representation for the object
appropriate for indexing. In particular, this means a deep merged
structure for node objects.


The code here is largely copied from the `chef_index` repo and the
`chef_index_queue` module, but isn't tied to rabbitmq client
libraries or actual queue publishing.
<a name="post_multi-1"></a>

### post_multi/1 ###


<pre><code>
post_multi(Commands::list()) -&gt; ok | {error, {term(), term()}}
</code></pre>

<br></br>


Given a list of command EJSON terms, as returned by [`make_command/4`](#make_command-4), perform the appropriate flatten/expand operation
and POST the result to Solr as a single update.
<a name="post_single-1"></a>

### post_single/1 ###


<pre><code>
post_single(Command::term()) -&gt; ok | {error, {term(), term()}}
</code></pre>

<br></br>


Given a command EJSON term as returned by [`make_command/4`](#make_command-4), flatten/expand and POST to Solr.
<a name="send_items-1"></a>

### send_items/1 ###


<pre><code>
send_items(Idx_exp_ctx::<a href="#type-index_expand_ctx">index_expand_ctx()</a>) -&gt; ok | {error, {term(), term()}}
</code></pre>

<br></br>


Send items accumulated in the index expand context to
Solr. The URL used to talk to Solr is embedded in the context
object and determined when <code><a href="#init_items-1"><code>init_items/1</code></a></code> was called.
