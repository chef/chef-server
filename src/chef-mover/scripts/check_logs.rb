#!/opt/opscode/embedded/bin/ruby
#
# @author Mark Anderson <mark@chef.io>
# @author Marc Paradise <marc@chef.io>
#
# Copyright 2013 Opscode Inc
# All Rights Reserved
#
# This script will serve two purposes:
# 1) parse the output console.log of a chef-mover migration
#    and determine if the migrations were successful or not based
#    on log file content.  Note that this can catch issues
#    that will not be captured as failing conditions at time of the migration.
# 2) output a condensed, more easily readable log file containing details of issues that it
#    does discover. This (plus the associated console/error logs) should be provided to
#    Opscode Support in the event of failed migration(s).
#
# Exit code:
#
# 0 - no issues that could cause data loss or corruption are detected.  Some non-critical
#     :warningings may have been captured but these are not expected to warrant further attention.
# 1 - issues that could result in (or be the result of) lost or corrupted data,
#     or if any error that is unknown to this script has been encountered.
#
# USAGE: ./check_logs.rb OUTPUT-NAME [INPUT-FILE|...]
#

class AllOrgStats
  def initialize()
    @stats = {}
  end

  def org(name)
    if not @stats[name]
      @stats[name] = OrgStats.new(name)
    end
    @stats[name]
  end

  def total(severity)
    total = 0
    @stats.each do |name, org|
      total += org.total(severity)
    end
    total
  end

  def dump()
    @stats.each do |name, org|
      org.dump()
    end
  end
end

class OrgStats
  attr_accessor :name

  def initialize(name)
    @name = name
    @counts = {}
    @failed_orgs = []
  end
  def incr(severity, obj_type)
    if not @counts[severity]
      @counts[severity] = {}
    end
    if @counts[severity][obj_type]
      @counts[severity][obj_type] = @counts[severity][obj_type] + 1
    else
      @counts[severity][obj_type] = 1
    end
  end

  def count(severity, obj_type)
    if @counts[severity] and @counts[severity][obj_type]
      @counts[severity][obj_type]
    else
      0
    end
  end

  def total(severity)
    result = 0
    if @counts[severity]
      @counts[severity].each {|where, value|  result += value }
    end
    result
  end

  def dump()
    puts "Organization: #{@name}"
    @counts.each do |severity, types|
      types.each do |obj_type, value|
        puts "    #{obj_type} #{severity}s: #{value}"
      end
    end
  end
end

UnknownOrg = "-UnknownOrg-"

IgnoredMessages = [
  /Application \S+ started on node 'mover@127.0.0.1'/,
  "Deprecated lager_file_backend config detected, please consider updating it",
  "Connecting to Rabbit at {127,0,0,1}:5672/chef (exchange: <<>>)",
  /Negotiated maximums: \(Channel = \d+, Frame = \d+, Heartbeat = \d+\)/,
  "RabbitMQ config missing. Indexing for search is disabled.",
  "Starting mosernormal []",
]

OkSkips = {}

def ignore_msg?(log_line)
  IgnoredMessages.any? do |matcher|
    case matcher
    when String then
      matcher == log_line
    when Regexp
      matcher.match log_line
    end
  end
end

def process_org_msg(orgname, details, desc, stats, output)
  return if desc.match(/Fallback lookup of user-side id/)
  if desc.match( /Starting migration/ )
    #output << "R #{orgname} ok general started \n"
    return
  end


  # LOG FAILURES
  #
  # 'INSERT LOG SKIP: cookbook_version cd4869e0-127d-4366-8a9e-12ab108fa458 2fcc5c2b309aafe15b136fb8aae8e76f c1c55268939e7b85e8bcf8a4ba4e78f9 git'
  if matches = desc.match( /INSERT LOG SKIP: (\S+) (\S+) (\S+) (\S+) (\S+)/)
    type, guid, v1, v2, name = matches.captures
    severity = OkSkips.has_key?(guid) ? ':warning' : 'error'
    output << "R #{orgname} #{severity} #{type} insert_log_skip #{name} #{guid}\n"
    return
  end

  # 'INSERT LOG FAIL: cookbook_version ec71bd30-ce6b-4456-bf8a-e7340d56a25d 52e447f3a991d8cc9635a0e1449d2318 5bdc710a98314ae259aea2010e9ba0a6 getting-started'
  # 'INSERT LOG FAIL: client 8c05149b7c02b464a28b37de345e4c71 8c05149b7c02b464a28b37de345e4c71 8a84b0ac4eb5540a9d2720e589df2aa5 scobal-inc-validator'
  if matches = desc.match( /INSERT LOG FAIL: (\S+) (\S+) (\S+) (\S+) (\S+)/)
    type, guid, v1, v2, name = matches.captures
    output << "R #{orgname} :error #{type} insert_log_fail #{name} #{guid}\n"
    return
  end

  severity = :error
  err_type = "unknown"
  #######################################################################
  # COOKBOOKS
#  if matches = desc.match( /cookbook FAILED_TO_VALIDATE .*Malformed cookbook name. Must only contain .*<<\"([^\"]*)\">> <<\"([^\"]*)\">>$/)
#    guid, name = matches.captures
#    output << "R #{orgname} :error cookbook validation_malformed_name #{name} #{guid}\n"
#    return
#  end

  # 'cookbook FAILED_TO_VALIDATE throw {ej_invalid,object_value,<<"metadata.platforms">>,<<"= 14">>,string,string,<<"Invalid version constraint">>} <<"443d910e-a14b-4046-861c-8e63f7a3296c">> <<"patch_fedora14_32bit_instance-0.0.0">>'

  # 'cookbook FAILED_TO_VALIDATE throw {ej_invalid,array_elt,<<"files">>,null,null,string,undefined} <<"af492e4a-29d7-4c3d-b013-1d8a87544236">> <<"opscode-ci-0.1.0">>'
  # 'cookbook FAILED_TO_VALIDATE throw {ej_invalid,exact,<<"name">>,<<"testabc">>,string,string,<<"testabc-0.1.0">>} <<"0d53f376-4e48-4bb4-be68-f5717bdbff95">> <<"testabc">>'
  if matches = desc.match( /cookbook FAILED_TO_VALIDATE (.+) <<\"([^\"]*)\">> <<\"([^\"]*)\">>$/)
    errorstr, guid, name = matches.captures
    err_desc = errorstr
    obj_type = "cookbook"
    case errorstr
    when /\{ej_invalid,(\S+),<<\"([^\"]*)\">>,<<\"?([^\"]*)\"?>>,string,string,<<\"Malformed cookbook name. Must only contain A-Z, a-z, 0-9, _ or -\">>\}/
      field, key, value = $1, $2, $3
      severity = :warning
      err_type = "malformed_name"
      output << "R #{orgname} #{severity} cookbook #{err_type} #{name} #{guid} #{key} #{value}\n"
    when /\{bad_cookbook_name,<<\"([^\"]*)\">>,<<\"Malformed cookbook name. Must only contain A-Z, a-z, 0-9, _ or -\">>\}/
      # these are acceptable, because these probably don't work anyways, or are minor quirks
      name = $1
      severity = :warning
      err_type = "empty_field"
      output << "R #{orgname} #{severity} cookbook #{err_type} #{name} #{guid} #{name}\n"
    when /\{ej_invalid,object_value,<<\"([^\"]*)\">>,<<\"([^\"]*)\">>,string,string,<<\"Invalid version constraint\">>\}/
      # these are acceptable, because these probably don't work anyways.
      key,value = $1, $2
      severity = :warning
      err_type = "invalid_version_constraint"
      output << "R #{orgname} #{severity} cookbook #{err_type} #{name} #{guid} #{key} #{value}\n"
    when /\{ej_invalid,array_elt,<<\"([^\"]*)\">>,null,null,string,undefined\}/
      # these are acceptable, because these probably don't work anyways, or are minor quirks
      key = $1
      severity = :warning
      err_type = "empty_field"
      output << "R #{orgname} #{severity} cookbook #{err_type} #{name} #{guid} #{key}\n"
    when /\{ej_invalid,exact,<<\"([^\"]*)\">>,<<\"([^\"]*)\">>,string,string,<<\"([^\"]*)\">>\}/
      # Known non-important error to have extended name here
      junk, name, wrongname = $1, $2, $3
      severity = :warning # (wrongname.index(name) == 0) ? "warn" : "error"
      err_type = "name_mismatch"
      output << "R #{orgname} #{severity} cookbook #{err_type} #{name} #{wrongname}\n"
    when /\{invalid_key,<<\"([^\"]*)\">>\}/
      # Known non-important error to excess key
      id =  $1
      severity = :warning
      err_type = "invalid_key"
      output << "R #{orgname} #{severity} cookbook #{err_type} #{name} #{guid} #{err_desc}\n"
    else
      output << "U3 #{orgname} #{severity} cookbook #{err_type} #{name} #{guid} #{err_desc}\n"
      return
    end
    stats.org(orgname).incr(severity, obj_type)
    return
  end



  #######################################################################
  # Cookbook versions
  # 'cookbook_version_missing_checksum cookbook_version application-0.99.11 (532895e2-25d3-40f1-9830-4cfbe2db358b) SKIPPED missing checksums'
  if matches = desc.match( /cookbook_version_missing_checksum cookbook_version (\S+) \((\S+)\) SKIPPED missing checksums/ )
    # we can :warning here, because it's probably a purged cookbook
    name, guid = matches.captures
    output << "R #{orgname} :warning cookbook_version version_missing_checksum #{name} #{guid}\n"
    OkSkips[guid] = :true
    stats.org(orgname).incr(:warning, "cookbook_version")
    return
  end

  # 'cookbook_version testabc (0d53f376-4e48-4bb4-be68-f5717bdbff95) SKIPPED {conflict,<<"duplicate key value violates unique constraint \"cookbook_versions_cookbook_id_major_minor_patch_key\"">>}'
  # 'cookbook_version testabc (1c53bc4d-1cf6-4bfa-9368-ce0ed8e4d679) SKIPPED {conflict,<<"duplicate key value violates unique constraint \"cookbook_versions_cookbook_id_major_minor_patch_key\"">>}'


  # 'cookbook_version FAILED error function_clause {{cookbook_version,<<"42b268a8-c3bc-4fff-8d7b-c60a14c0f8be">>},...'
  if matches = desc.match(/cookbook_version FAILED error function_clause/)
    output << "R #{orgname} :error cookbook_version function_clause_failed\n"
    stats.org(orgname).incr(:error, "cookbook_version")
    return
  end

  #######################################################################
  # Roles
  # 'role FAILED_TO_VALIDATE throw {ej_invalid,array_elt,<<"run_list">>,<<"recipe[]">>,string,string,<<"Invalid run list entry">>} <<"8f02bed4-af53-4e49-b040-dbe0b804aa17">> <<"Role1">>'
  if matches = desc.match( /role FAILED_TO_VALIDATE .*<<\"run_list\">>,<<\"([^"]*)\">>.*<<\"([^\"]*)\">> <<\"([^\"]*)\">>$/)
    runlist, guid, name = matches.captures
    # these are likely to cause failed convergences, so unlikely real infra depends on this.
    output << "R #{orgname} :warning role validation_malformed_runlist #{name} #{guid} #{runlist}\n"
    stats.org(orgname).incr(:warning, "role")
    return
  end

  # 'role FAILED_TO_VALIDATE throw {invalid_key,<<"recipes">>} <<"9ba5973b-0054-4632-a8d3-ec3e9994ef31">> <<"database">>'
  if matches = desc.match( /role FAILED_TO_VALIDATE .*\{invalid_key,<<\"([^"]*)\">>\} <<\"([^\"]*)\">> <<\"([^\"]*)\">>$/)
    value, guid, name = matches.captures
    output << "R #{orgname} :warning validation_invalid_key #{name} #{guid} #{value}\n"
    stats.org(orgname).incr(:warning, "role")
    return
  end

  # 'role FAILED_TO_VALIDATE throw {ej_invalid,object_value,<<"env_run_lists">>,<<"recipe[php:module_xdebug]">>,string,string,<<"Invalid run list entry">>} <<"a2b4845d-3fcc-4a1d-90a1-9adc5ce88dd7">> <<"mylamp">>'
  if matches = desc.match( /role FAILED_TO_VALIDATE throw \{ej_invalid,object_value,<<\"([^"]*)\">>,<<\"([^"]*)\">>,string,string,<<"Invalid run list entry">>\} <<\"([^\"]*)\">> <<\"([^\"]*)\">>$/)
    key, value, guid, name = matches.captures
    output << "R #{orgname} :error role validation_invalid_run_list #{name} #{guid} #{key} #{value}\n"
    stats.org(orgname).incr(:error, "role")
    return
  end

  # 'No such environment <<"ops-testing">>'
  if matches = desc.match( /No such environment <<\"([^"]*)\">>/ )
    name = matches.captures[0]
    output << "R #{orgname} :warning environment missing #{name}\n"
    stats.org(orgname).incr(:warning, "environment")
    return
  end


  #######################################################################
  # alternate validation failure type
  # '{environment,object_key,<<"cookbook_versions">>,<<"users::sysadmins">>} FAILED <<"Malformed cookbook name. Must only contain A-Z, a-z, 0-9, _ or -">>
  if matches = desc.match( /\{(\S+),(\S+),<<\"(\S+)\">>,<<\"([^"]*)\">>\} FAILED <<\"Malformed \S+ name. Must only contain A-Z, a-z, 0-9, _ or -\">>/ )
    type, subtype, key, value = matches.captures
    output << "R #{orgname} :error #{type} malformed_#{subtype} #{key}, #{value}\n"
    stats.org(orgname).incr(:error, type)
    return
  end

  # 'authz_id_not_found SKIPPING client bamboo-upload (39bb9fe2377508819902c010e7380554) missing authz data'
  if matches = desc.match( /authz_id_not_found SKIPPING (\S+) (\S+) \((\S+)\) missing authz data/)
    # THIS IS marked as :warning, because this is likely non-working
    type, name, guid = matches.captures
    output << "R #{orgname} :warning #{type} authz_id_not_found #{name} #{guid}\n"
    stats.org(orgname).incr(:warning, type)
    return
  end

  # 'user_side_authz_not_found SKIPPING databag passwords (2dc3dbff-973b-4676-8efe-71cb283237f2) missing authz data'
  if matches = desc.match( /user_side_authz_not_found SKIPPING (\S+)\s+(\S*)\s+\((\S+)\) missing authz data/ )
    # THIS IS marked as :warning, because this is likely an orphan object
    type, name, guid = matches.captures
    stats.
    output << "R #{orgname} :warning #{type} user_side_authz_not_found #{name} #{guid}\n"
    stats.org(orgname).incr(:warning, type)
    return
  end

  #######################################################################
  # constraint violations
  # 'cookbook_version testabc (0d53f376-4e48-4bb4-be68-f5717bdbff95) SKIPPED {conflict,<<"duplicate key value violates unique constraint \"cookbook_versions_cookbook_id_major_minor_patch_key\"">>}'
  if matches = desc.match( /(\S+) (\S+) \((\S+)\) SKIPPED \{conflict,<<\"duplicate key value violates unique constraint \\\"(\S+)\\\"\">>/ )
    type, name, guid, constraint = matches.captures
    output << "R #{orgname} :error #{type} db_constraint_duplicate #{name} #{guid} #{constraint}\n"
    stats.org(orgname).incr(:error, type)
    return
  end

  # '{role,chef_sql} FAILED {{conflict,<<"duplicate key value violates unique constraint \"roles_authz_id_key\"">>}...'
  if matches = desc.match( /\{(\S+),(\S+)\} FAILED \{\{conflict,<<\"duplicate key value violates unique constraint \\\"(\S+)\\\"\">>/ )
    type, location, constraint = matches.captures
    output << "R #{orgname} :error #{type} db_constraint_duplicate #{name} #{constraint}\n"
    stats.org(orgname).incr(:error, type)
    return
  end

  # '{badmatch,{error,{error,{error,error,<<"23505">>,<<"duplicate key value violates unique constraint \"checksums_pkey\"">>,[{detail,<<"Key (org_id, checksum)=(b34497481daa43fe9c7178e8bfa57577, 201c1cdbb2b86c534cb859d149a7b5eb) already exists.">>}]}}}}'
  if matches = desc.match( /duplicate key value violates unique constraint \\\"([^"]*)\\\"\">>,\[\{detail,<<\"([^"]*)\">>\}/ )
    constraint, detail = matches.captures
    output << "R #{orgname} :error unknown db_constraint_duplicate_crash #{constraint} #{detail}\n"
    stats.org(orgname).incr(:error, constraint)
    return
  end

  # '{databag_item,chef_sql} FAILED {{foreign_key,<<"insert or update on table \"data_bag_items\" violates foreign key constraint \"data_bag_items_org_id_fkey\"">>},<<"6acaedbf-3e67-4ea0-8850-667d9bb1ad54">>,<<"unset">>} ...'
  if matches = desc.match(/.*violates foreign key constraint \\\"data_bag_items_org_id_fkey\\\".*databag_item,<<\"([^"]*).*name\">>,<<"([^"]*)".*data_bag\">>,<<\"([^"]*)"/)
    item_id, item_name, data_bag_name = $1, $2, $3
    output << "R #{orgname} :error data_bag_item db_constraint_duplicate #{data_bag_name} #{item_name} #{item_id}\n"
    stats.org(orgname).incr(:error, "data_bag_item")
    return
  end
  # If the item is long enough, it will be ellided and we'll be missing some data - so catch what we can.
  # to catch it anyway.
  if matches = desc.match(/.*violates foreign key constraint \\\"data_bag_items_org_id_fkey\\\".*databag_item,<<\"([^"]*).*name\">>,<<"([^"]*)/)
    item_id, item_name = $1, $2
    output << "R #{orgname} :error data_bag_item db_constraint_duplicate #{UnknownOrg} #{item_name} #{item_id}\n"
    stats.org(orgname).incr(:error, "data_bag_item")
    return
  end

  # SYSTEM ERROR
  # 'build-essential-1.0.0 (9e313ae0-f6df-4d63-9b08-8fd3f606eeea) SKIPPED {error,no_connections}'
  if matches = desc.match( /(\S+) \((\S+)\) SKIPPED \{error,no_connections\}/)
    name, guid = matches.captures
    output << "R #{orgname} :error unknown no_connections #{name} #{guid}\n"
    stats.org(orgname).incr(:error, "system")
    return
  end

  # 'Error in converting org data, skipping:  {error,not_found}'
  if matches = desc.match( /Error in converting org data, skipping:  \{error,not_found\}/ )
    output << "R #{orgname} :error unknown not_found\n"
    stats.org(orgname).incr(:error, "system")
    return
  end


  # Some other log strings that we will default to treating as an error by not handling them here.
  #
  # Can't open file '/srv/couch-data/databases/chef_cfcdff1818ad4456ae024dc43ebcb45d.couch'<
  # Error in converting org data, skipping:  {no_such_file,<<"/srv/couch-data/databases/chef_cfcdff1818ad4456ae024dc43ebcb45d.couch">>}<
  # unclassified Node unexpectedly not found... <<"node">>
  # tried to add 1 members, only added 0
  # Unable to start database connection
  # Starting purge of
  # <0.133.0> terminated with reason
  # Supervisor {<0.96.0>,pooler_pooled_worker_sup} had child sqerl_client s

  stats.org(orgname).incr(:error, "unclassified")
  output << "U2 #{orgname} :error unclassified #{desc}\n"
end

def process_file(file, stats, output)

  File.open file do |f|
    f.each_line do |l|
      l.chomp!
      next if l.match(/^\s*$/)

      desc, message = nil
      time, type, orgname, guid = nil

      next if l.match( /INSERT LOG OK/ )

      next if l.match( /checksum_time/ )
      next if l.match( /Insert_time/ )
      next if l.match( /Insert databag Stats/ )
      next if l.match( /databag insertions took/ )
      next if l.match( /Insert object Stats/ )
      next if l.match( /object insertions took/ )
      next if l.match( /inserts complete/ )
      next if l.match( /READ \S+ COMPLETED/ )

      # ignored, different tool
      next if l.match(/Starting dep validation./)
      next if l.match(/Starting.*\[depsolver\]$/)
      next if l.match(/total.*unreachable.*timeout.*/)
      next if l.match(/<<[^>]*>>: \{\d+,\d+,\d+/)
      next if l.match(/Terminating after successful validation/)

      # Artifact of recent sqerl changes, should not occur in
      # more recent builds of mover, but not harmful if we see it.
      next if l.match(/deprecated_application_config,sqerl,db_type/)

      # We will see this during an attempt to re-process migration in EC
      next if l.match(/org already exists, ignoring/)
      next if l.match(/Requesting migration/)

      # we're  only interested in things that have failed.
      next if l.match(/Terminating after successful migration/)

      # if the files don't exist, we already handle creation of them.
      next if l.match(/dets_file_not_found,mover_manager/)
      next if l.match(/Failed to verify idle connection/)

      # failed to find org occurs when an org has been deleted. This is ignorable.
      next if l.match(/Failed to find org/)

      # we are generating a garbage field '110111951111141033240110111951051004158' or similar instead of org name sometimes
      if matches = l.match( /^(\S+)\s+(\S+)\s+\[(\S+)\]\s+\d{15,}(.*)$/)
        date, time, type, msg = matches.captures
        # ignore noise from tool
        next if ignore_msg?(msg)

        if matches = msg.match( /org_not_found Failed to find org:\s+\[<<\"([a-z0-9_\-]*)\">>\]\s*$/ )
          orgname = matches.captures[0]
          output << "R #{orgname} :error general org_not_found\n"
          stats.org(orgname).incr(:error, "general")
          next
        end

        if matches = msg.match( /Terminating after failed migration of\s+\[<<\"([a-z0-9_\-]*)\">>\]\s*$/ )
          orgname = matches.captures[0]
          output << "R #{orgname} :error general terminated\n"
          stats.org(orgname).incr(:error, "general")
          next
        end

        if matches = msg.match( /Failed to verify org state: \[<<\"([a-z0-9_\-]*)\">>\] "started" Error: no_connections\s*$/ )
          orgname = matches.captures[0]
          output << "R #{orgname} :error general no_connections\n"
          stats.org(orgname).incr(:error, "general")
          next
        end

        # Just ot be sure nothing slips through the cracks we'll capture these as well as 'erlang'
        # Skip erlang errors (but typically preceded by other errors that we do capture)
        if msg.match( /int halting: Worker down with unexpected error/ ) or
           msg.match( /CRASH REPORT/ ) or
           msg.match( /Supervisor mover_org_migrator_sup had child undefined started with/ )
           msg.match( /UPDATE FAILED:/ )
           msg.match( /gen_fsm/ )
           output << "R #{UnknownOrg} :error erlang #{msg}"
           stats.org(UnknownOrg).incr(:error, "system")
          next
        end

        stats.org(UnknownOrg).incr(:error, "general")
        output << "U1 #{UnknownOrg} :error general #{msg}\n"
        next;
      end

      # '2013-06-19 19:16:38.601 [info] travisberry (004f50c0666744daa34dd6fd37ef5458): asdfasdfasdfasdf'
      # '2013-06-19 19:15:26.678 [info] practice9 (Undefined): Starting migration'

      if matches = l.match( /^(\S+)\s+(\S+)\s+\[(\S+)\]\s+([a-z0-9_\-]*)\s+\((\S+)\):\s+(.*)$/ )
        date, time, type, orgname, guid, msg = matches.captures
        process_org_msg(orgname, [date,time,type,guid], msg, stats, output)
        next;
      end
      stats.org(UnknownOrg).incr(:error, "general")
      output << "U0 :error general #{l}\n"
    end
  end
  #stats
end


if ARGV.length < 2
  puts "USAGE: ./check_logs.rb OUTPUT-NAME [INPUT-FILE|...]"
else
  stats = AllOrgStats.new()
  output = File.open(ARGV.shift, "w")
  ARGV.each {|x| process_file(x, stats, output) }
  output.close
  stats.dump()
  if stats.total(:error) > 0
    exit 1
  else
    exit 0
  end
end


