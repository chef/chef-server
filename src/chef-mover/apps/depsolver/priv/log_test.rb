#!/usr/bin/env ruby
# Copyright 2012-2018 Chef Software, Inc.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
require 'pp'
require 'rubygems'
require 'dep_selector'

include DepSelector

ADD_PKG=/^DepSelector\sinst#\s(\d+)\s-\sAdding\spackage\sid\s(\d+)\/(\d+):\smin\s=\s-1,\smax\s=\s(\d+),\scurrent\sversion\s0$/
ADD_VC=/^DepSelector\sinst#\s(\d+)\s-\sAdding\sVC\sfor\s(\d+)\s@\s(\d+)\sdepPkg\s(\d+)\s\[\s(\d+)\s(\d+)\s\]$/
ADD_GOAL=/^DepSelector\sinst#\s(\d+)\s-\sMarking\sPackage\sRequired\s(\d+)$/

def parse_package(cache, dep_graph, conline)
  matchData = ADD_PKG.match(conline)
  if !matchData
    return
  end
  pkgName = matchData[2]
  maxVsn = Integer(matchData[4])

  pkg = dep_graph.package(pkgName)
  cache[pkgName] = pkg
  for i in (0..maxVsn)
    vsn = String(i)
    pkgVsn = pkg.add_version(Version.new("0." + vsn))
    cache[pkgName + ":" + vsn] = pkgVsn
  end
end

def parse_version_constraint(cache, conline)
  matchData = ADD_VC.match(conline)
  if !matchData
    return
  end
  pkgName = matchData[2]
  pkgVsn = "0." + matchData[3]
  depPkgName = matchData[4]
  depPkgVsn = "0." + matchData[6]

  depPkg = cache[depPkgName]
  pkg = cache[pkgName + ":" + pkgVsn]
  pkg.dependencies << Dependency.new(depPkg,
                                     VersionConstraint.new("= " + depPkgVsn))
end

def parse_goal(cache, dep_graph, conline)
  matchData = ADD_GOAL.match(conline)
  if !matchData
    return
  end
  pkgName = matchData[2]
  cache.push(SolutionConstraint.new(dep_graph.package(pkgName)))
end

def process_file(filename)

  dep_graph = DependencyGraph.new
  conline = nil
  package_cache = {}
  goal_cache = []

  File.open(filename, "r") do |infile|
  #get the constraint line and parse it

    while (line = infile.gets)
      parse_package(package_cache, dep_graph, line)
      parse_version_constraint(package_cache, line)
      parse_goal(goal_cache, dep_graph, line)
    end
  end

  selector = Selector.new(dep_graph)
  beginning_time = Time.now

  begin
    result = selector.find_solution(goal_cache)
    end_time = Time.now
    pp result
  rescue Exceptions::InvalidSolutionConstraints => isc
    end_time = Time.now
    puts "non-existent solution constraints: #{isc.non_existent_packages.join(', ')}"
    puts "solution constraints whose constraints match no versions of the package: #{isc.constrained_to_no_versions.join(', ')}"
  end
  puts "Time elapsed #{(end_time - beginning_time)*1000} milliseconds"
end

process_file(ARGV[0])
