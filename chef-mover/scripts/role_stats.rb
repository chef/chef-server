require 'time'

def get_role(org_name, role_name)
  db = ORGS.database_from_orgname(org_name)
  the_id = db.view('roles/all_id')["rows"].find { |row| row["key"] =~ /#{role_name}/ }["id"]
  db.get(the_id)
end

def role_stats_for_org(org_name)
  role_stats(fetch_all_roles(org_name))
end

def fetch_all_roles(org_name)
  db = ORGS.database_from_orgname(org_name)
  db.view('roles/all_id', :include_docs => true)["rows"]
rescue
  # The view may be missing for unassigned orgs.
  # Also, an unassigned org may get assigned/renamed and no longer exist.
  []
end

def role_stats0(all_roles)
  count = 0
  max = 0
  total_size = 0
  all_roles.each do |r|
    count += 1
    rj = r.to_json
    rsize = rj.bytesize
    total_size += rsize
    if rsize > max
      max = rsize
    end
  end
  { :count => count, :max => max, :total_size => total_size }
end

def role_stats(org_name)
  all_roles = fetch_all_roles(org_name)
  ans = []
  all_roles.each do |doc|
    role = doc["doc"]
    rsize = role.to_json.bytesize
    ans << [org_name, role.name, rsize]
  end
  ans
end

def stats_header
  ["org_name", "role_name", "role_size"].join("\t")
end

def write_stats_for_org(fh, stats)
  stats.each do |stat|
    fh.write("%s\t%s\t%d\n" % stat)
  end
end

def collect_role_stats(file="/tmp/org_role_size.txt")
  puts "starting role stats collection"
  puts Time.now
  i = 0
  open(file, "w") do |fh|
    fh.write(stats_header + "\n")
    ORGS.all_names.each do |org_name|
      i += 1
      org_stats = role_stats(org_name)
      write_stats_for_org(fh, org_stats)
      if i % 1000 == 0
        print "."
      end
    end
  end
  puts "\ndone"
  puts Time.now
end
