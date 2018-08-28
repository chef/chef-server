Depsolver
=========

Overview
--------
`Depsolver` is a dependency solver package for erlang. You supply it
with a list of versioned objects that are in the `world` and then you
can solve for different version constraints.

If no solution can be found, detailed back tracking information is
returned that can be used to detect what exact dependencies are causing
the problem.

Building
--------
1) In order to build you need rebar (https://github.com/basho/rebar)

2) run `make`

API
-----
Lets say our world looks as follows

    app1 that has versions "0.1"
      depends on app3 any version greater then "0.2"
     "0.2" with no dependencies
     "0.3" with no dependencies

    app2 that has versions "0.1" with no dependencies
     "0.2" that depends on app3 exactly "0.3"
     "0.3" with no dependencies

    app3 that has versions
     "0.1", "0.2" and "0.3" all with no dependencies

We can add this world to the system all at once as follows

    Graph0 = depsolver:new_graph(),
    Graph1 = depsolver:add_packages(
           [{app1, [{"0.1", [{app2, "0.2"},
                             {app3, "0.2", '>='}]},
                             {"0.2", []},
                             {"0.3", []}]},
            {app2, [{"0.1", []},
                     {"0.2",[{app3, "0.3"}]},
                     {"0.3", []}]},
            {app3, [{"0.1", []},
                    {"0.2", []},
                    {"0.3", []}]}]).

We can also build it up incrementally using the other add_package and
add_package_version functions.

Finally, once we have built up the graph we can ask depsolver to solve the
dependency constraints. That is to give us a list of valid dependencies by
using the solve function. Lets say we want the app3 version "0.3" and all of
its resolved dependencies. We could call solve as follows.

    depsolver:solve(Graph1, [{app3, "0.3"}]).

That will give us the completely resolved dependencies including app3
itself. Lets be a little more flexible. Lets ask for a graph that is rooted
in anything greater then or equal to app3 "0.3". We could do that by

    depsolver:solve(Graph1, [{app3, "0.3", '>='}]).

Of course, you can specify any number of goals at the top level.

License and Authors
----------

* Author:: Eric Merritt (<ericbmerritt@gmail.com>)
* Author:: James Casey (<james@chef.io>)

Copyright 2012-2018 Chef Software, Inc.

This file is provided to you under the Apache License,
Version 2.0 (the "License"); you may not use this file
except in compliance with the License.  You may obtain
a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
