open_source_11_sv_dir = "/opt/chef-server/sv"

return unless Dir.exist?(open_source_11_sv_dir)

# find all of the directory entries in the sv_dir that are
# not hidden, '..', or '.'.
services = Dir.new(open_source_11_sv_dir).entries.select do |d|
  File.directory?(File.expand_path(d, open_source_11_sv_dir)) &&
    !/\..*/.match(d)
end

services.each do |sv_entry|
  sv_dir = File.expand_path(sv_entry, open_source_11_sv_dir)
  down_file = File.expand_path("down", sv_dir)
  file down_file do
    owner "root"
    group "root"
    mode   0644
    content ""
  end
end
