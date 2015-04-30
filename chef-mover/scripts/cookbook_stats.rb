require 'time'

def fetch_all_cookbook_recipe_names(org_name)
  db = ORGS.database_from_orgname(org_name)
  all_cookbooks = db.view('cookbooks/all')["rows"].map { |x| x["value"] }
  all_names = all_cookbooks.map { |cb| cb.name }
  all_cookbooks.each do |cb|
    all_names += cb.recipe_filenames_by_name.keys
  end
  unique_names_set = Set.new(all_names)
  unique_names_set.to_a
rescue
  # The view may be missing for unassigned orgs.
  # Also, an unassigned org may get assigned/renamed and no longer exist.
  []
end

def write_stats_for_org(fh, all_names)
  all_names.each do |stat|
    fh.write("%s\n" % stat)
  end
end

def collect_cookbook_name_stats(file="/tmp/all_cookbook_recipe_names.txt")
  puts "starting cookbook recipe name collection"
  puts Time.now
  i = 0
  open(file, "w") do |fh|
    ORGS.all_names.each do |org_name|
      i += 1
      all_names = fetch_all_cookbook_recipe_names(org_name)
      write_stats_for_org(fh, all_names)
      if i % 1000 == 0
        print "."
      end
    end
  end
  puts "\ndone"
  puts Time.now
end
