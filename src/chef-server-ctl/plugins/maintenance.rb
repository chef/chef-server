
require "redis"
require "resolv"

def redis
  @redis ||= begin
               vip = running_config["private_chef"]["redis_lb"]["vip"]
               port = running_config["private_chef"]["redis_lb"]["port"]
               password = credentials.get('redis_lb', 'password')
               Redis.new(port: port, ip: vip, password: password)
             end
end

def enable_maintenance
  puts "- Enabling maintenance mode"
  redis.hset("dl_default", "503_mode", true)
end

def disable_maintenance
  puts "- Disabling maintenance mode"
  redis.hdel("dl_default", "503_mode")
end

def list_allowed_ips()
  ips = redis.smembers("xmaint_allowed_ips_list")
  if ips.length == 0
    puts "- allowed IP list is empty."
  else
    puts "- The following IPs are allowed in the maintenance mode \n", ips
  end
end

def add_ip_to_allowed_list(ip)
  validate_ip(ip)
  puts "- Adding #{ip} to allowed list."
  redis.sadd("xmaint_allowed_ips_list", ip)
end

def remove_ip_from_allowed_list(ip)
  validate_ip(ip)
  puts "- Removing #{ip} from allowed list."
  redis.srem("xmaint_allowed_ips_list", ip)
end

def validate_ip(ip)
  isIPv4 = ip =~ Resolv::IPv4::Regex ? true : false
  isIPv6 = ip =~ Resolv::IPv6::Regex ? true : false
  unless isIPv4 or isIPv6
    $stderr.puts "The IP address is invalid, please enter valid IP address"
    exit 1
  end
end


add_command_under_category "maintenance", "general", "Handel the server properly while maintenance", 2 do
  args = ARGV[1..-1] # Chop off first 1 args, keep the rest... that is, everything after "chef-server-ctl maintenance"
  options = {}

  case args[0]
    when 'on'
      enable_maintenance
      args = args[1..-1]
    when 'off'
      disable_maintenance
      args = args[1..-1]
    else
      if args.length == 0
        $stderr.puts "Please specify the action 'on' or 'off' or pass the maintenance options"
        exit 1
      end
  end

  OptionParser.new do |opts|

    opts.on("-l", "--list-ips", "List the allowed IPs list") do
      options[:list_ip] = true
    end

    opts.on("-a", "--add-ip [String]", "Add an IP to allowed list") do |a|
      options[:add_ip] = a
    end

    opts.on("-r", "--remove-ip [String]", "Remove an IP from the allowed list") do |a|
      options[:remove_ip] = a
    end

    opts.on("-h", "--help", "Print this help message") do
      puts opts
      exit 1
    end
  end.parse!(args)

  list_allowed_ips() if options[:list_ip]
  add_ip_to_allowed_list(options[:add_ip]) if options[:add_ip]
  remove_ip_from_allowed_list(options[:remove_ip]) if options[:remove_ip]

end