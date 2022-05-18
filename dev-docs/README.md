The goal of these documents is to provide overviews of how various systems work inside Chef Server. If you encounter a process or feature that isn’t straightforward or took you time to understand consider writing an explanation here.

 * [High Level Architecture](ChefServer14HighLevelDiagram.jpeg)
 * [Search and Indexing](SEARCH_AND_INDEXING.md)
 * [Cookbook Upload Request Sequence Diagram](ChefServer14CookbookUploadflow.jpeg)
 * [Permissions Model](ChefServerPermissions_v1.3.pdf) - covers overall model including authz, authn
 * [Build and Test Pipelines](BUILD_AND_TEST_PIPELINES.md)
 * [FIPS](FIPS.md) - FIPS-mode support
 * [Development Environment Setup and Configuration](../dev/README.md)



## Undocumented

This is an ongoing list of documentation we would like to add. Please feel free to add more topics, or take on documenting one of them.


#### Secrets Management

* what is veil and how do we use it to keep internal passwords secure?
* which components use it?

#### API Request Flow

* What does an API request look like, from client to all of the back end components, and everything in between.
* What components and services get used? What is sync/async in that handling?

#### Automate Integration

* interaction diagram
* data collector - when/where/what data do we supply?
* authentication and configuration

#### Cookbook Dependency Solving

* definition of what it is, when/why it gets performed.
* How are depsolver requests handled, from end-to-end
* what are the depsolver components? (erlang, ruby, native gecode binding)

#### Pools

* How do we use `pooler`?
* What problems is it solving for us?
* Which components in Server use it?
* What does the flow look like for services

#### object model
The chef_object and chef_db model for managing, retrieving, and persisting Infra Server objects.

#### Metrics
Server has support for reporting into 2 or 3 different application metrics systems. Document:
* What they are
* What kind of metrics do we capture
* What components capture, and into which systems?
