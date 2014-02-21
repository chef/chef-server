

# Module chef_index_queue #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-chef_db_name">chef_db_name()</a> ###



<pre><code>
chef_db_name() = binary()
</code></pre>





### <a name="type-ejson">ejson()</a> ###



<pre><code>
ejson() = {maybe_improper_list()}
</code></pre>



  Is an ejson object, but the type defn for that is recursive.



### <a name="type-uuid_binary">uuid_binary()</a> ###



<pre><code>
uuid_binary() = &lt;&lt;_:288&gt;&gt; | &lt;&lt;_:256&gt;&gt;
</code></pre>



  with|without hypens are both allowed
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-3">delete/3</a></td><td>Delete an object from Solr.</td></tr><tr><td valign="top"><a href="#package_for_delete-3">package_for_delete/3</a></td><td>wraps a chef object in the necessary envelopes for expander to de-index it.</td></tr><tr><td valign="top"><a href="#package_for_set-4">package_for_set/4</a></td><td>wraps a chef object in the necessary envelopes for expander to index it.</td></tr><tr><td valign="top"><a href="#set-4">set/4</a></td><td>Insert or update an object in Solr.</td></tr><tr><td valign="top"><a href="#unix_time-0">unix_time/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-3"></a>

### delete/3 ###


<pre><code>
delete(Type::<a href="#type-chef_indexable_type">chef_indexable_type()</a>, ID::<a href="#type-uuid_binary">uuid_binary()</a>, DatabaseName::<a href="#type-chef_db_name">chef_db_name()</a>) -&gt; ok
</code></pre>

<br></br>


Delete an object from Solr.
<a name="package_for_delete-3"></a>

### package_for_delete/3 ###


<pre><code>
package_for_delete(Type::<a href="#type-chef_indexable_type">chef_indexable_type()</a>, ID::<a href="#type-uuid_binary">uuid_binary()</a>, DatabaseName::<a href="#type-chef_db_name">chef_db_name()</a>) -&gt; {[{action, delete} | {payload, {term()}}, ...]}
</code></pre>

<br></br>


wraps a chef object in the necessary envelopes for expander to de-index it.
<a name="package_for_set-4"></a>

### package_for_set/4 ###


<pre><code>
package_for_set(Type::<a href="#type-chef_indexable_type">chef_indexable_type()</a>, ID::<a href="#type-uuid_binary">uuid_binary()</a>, DatabaseName::<a href="#type-chef_db_name">chef_db_name()</a>, Item::<a href="#type-ejson">ejson()</a>) -&gt; {[{action, add} | {payload, {term()}}, ...]}
</code></pre>

<br></br>


wraps a chef object in the necessary envelopes for expander to index it.
<a name="set-4"></a>

### set/4 ###


<pre><code>
set(Type::<a href="#type-chef_indexable_type">chef_indexable_type()</a>, ID::<a href="#type-uuid_binary">uuid_binary()</a>, DatabaseName::<a href="#type-chef_db_name">chef_db_name()</a>, Item::<a href="#type-ejson">ejson()</a>) -&gt; ok
</code></pre>

<br></br>


Insert or update an object in Solr.
<a name="unix_time-0"></a>

### unix_time/0 ###

`unix_time() -> any()`


