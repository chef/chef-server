#!/usr/bin/ruby
#
# Couchdb health check.
#
# Presently, it checks for ' 404' errors, and verifies if this is the missing view problem.
# TODO
#   Look for other kinds of errors, including erlang crash dumps.
#


require 'rubygems'
require 'json'


def scan_log_for_view_errors(file)
  fails = {}
  if (file.class == Array) 
    errs = `grep ' 404$' #{file.join(' ')}` 
  else
    errs = `grep ' 404$' #{file}` 
  end
  errs.each_line do |l|
    elems = l.split(' ')
    resource = elems[7]
    next if elems[3] == '127.0.0.1' # filter out our own queries
    if (resource =~ /(.*\/_design\/.*\/_view\/[^\?]*)/)
      view = Regexp.last_match(1)
#      puts "#{view}"
      fails[view] = true
    end
  end
  fails.keys
end

def check_views(failed_views)
  failed_checks = {}
  failed_views.each do |view|
    begin 
      result = `curl -s 'localhost:5984#{view}?limit=0'`
      vdata = JSON.parse(result) 
      # only good result has a 'total rows' field
      if (!vdata.has_key?('total_rows')) 
        failed_checks[view] = true
      end
    rescue
      failed_checks[view] = true
    end
  end
  if (failed_checks.length > 0) 
    puts "Couch View Check FAIL: #{failed_checks.keys.join(', ')}"
    exit(2)
  else
    if (failed_views.length == 0) 
      puts "Couch View Check OK - no views with 404's"
    else
      puts "Couch View Check OK - all #{failed_views.length} views with 404's responding: " + (failed_views.map { |x| "#{x}"}).join(" ")
    end
    exit(0)
  end
end

failures = []
file = ARGV[0] || '/etc/sv/couchdb/log/main'
if (FileTest.directory?(file)) 
  files =Dir.foreach(file).map { |f| File.join(file,f) } .select { |f| File.file?(f) && File.readable?(f) }
  failures = scan_log_for_view_errors(files)
else
  failures = scan_log_for_view_errors(file)
end


check_views(failures)
