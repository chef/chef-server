

add_command_under_category "filtered-dump", "Debug Tools", "Generate a filtered dump of indexable Chef Objects for all organizations. Top-level data is captured; only object name is captured from object json" do
  require "json"
  require "zlib"
  require "stringio"

  conn = ::PG::Connection.open(::ChefServerCtl::Config.erchef_sql_connuri)
  File.write("orgs.json", organizations(conn).to_json)
  File.write("nodes.json", nodes(conn).to_json)
  File.write("data_bag_items.json", data_bag_items(conn).to_json)
  File.write("environments.json", environments(conn).to_json)
  File.write("roles.json", roles(conn).to_json)
  File.write("clients.json", clients(conn).to_json)
  conn.close
end

def organizations(conn)
  puts "Exporting organizations"
  # Map to force it to array instead of PGresult
  conn.exec_params("select id, name, created_at, updated_at from orgs order by id").map { |r| r }
end

def nodes(conn)
  puts "Exporting nodes"
  node_data = conn.exec_params("select org_id, id, name, environment, created_at, updated_at, serialized_object from nodes order by org_id")
  node_data.map do |n|
    clean_serialized("node", conn, n)
  end
end

def data_bag_items(conn)
  puts "Exporting data bag items"
  data_bag_items = conn.exec_params("select org_id, id, data_bag_name, item_name, created_at, updated_at, serialized_object from data_bag_items order by org_id")
  data_bag_items.map do |d|
    clean_data_bag_item(conn, d)
  end
end

def environments(conn)
  puts "exporting environments"
  env_data = conn.exec_params("select org_id, id, name, created_at, updated_at, serialized_object from environments order by org_id")
  env_data.map do |e|
    clean_serialized("environment", conn, e)
  end
end

def roles(conn)
  puts "exporting roles"
  role_data = conn.exec_params("select org_id, id, name, created_at, updated_at, serialized_object from roles order by org_id")
  role_data.map do |r|
    clean_serialized("role", conn, r)
  end
end

def clients(conn)
  puts "exporting clients"
  client_data = conn.exec_params("select org_id, id, name, validator, admin, created_at, updated_at from clients order by org_id")
  client_data.map do |c|
    c
  end
end

def clean_serialized(type, conn, object)
  serialized = inflate(conn, object["serialized_object"])
  begin
    h = JSON.parse(serialized)
    clean_attributes(h)
    object["serialized_object"] = h
  rescue
    object["serialized_object"] = { "error" => "could not parse #{type} serialized_object as json" }
  end
  if object["serialized_object"]["name"] != object["name"]
    puts "Warning: #{type} name mismatch - #{object["name"]} - #{object["serialized_object"]["name"]}"
  end
  object
rescue => e
  puts "Warning: unexpected error #{e.message} trying to parse serialized_data for #{object["id"]}"
  object["serialized_object"] = "unknown"
  object
end

def clean_data_bag_item(conn, d)
  o = JSON.parse(inflate(conn, d["serialized_object"]))
  o.each do |k, v|
    next if k == "id"

    o[k] = "value removed"
  end
  if o["id"] != d["item_name"]
    puts "Warning: data bag item name mismatch: #{o["id"]} - #{d["item_name"]}"
  end
  d["serialized_object"] = o
  d
end

def clean_attributes(hash)
  %w{default_attributes normal_attributes override_attributes automatic_attributes
     normal default override automatic}.each { |v| hash[v] = "removed #{hash[v].length} entries" if hash.has_key? v }
end

def inflate(conn, data)
  gz = Zlib::GzipReader.new(StringIO.new(conn.unescape_bytea(data)))
  gz.read
rescue
  # not in gz format
  data
end
