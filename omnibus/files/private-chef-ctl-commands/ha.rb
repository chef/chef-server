#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

add_command_under_category "master-recover", "high-availability", "Set this server to HA master state, ignoring VRRP.", 1 do
  exec "/var/opt/opscode/keepalived/bin/cluster.sh master"
end

add_command_under_category "backup-recover", "high-availability", "Set this server to HA backup state, ignoring VRRP", 1 do
  exec "/var/opt/opscode/keepalived/bin/cluster.sh backup"
end

#
# ha-status
#  - check keepalived is actually enabled in the config
#  - check that the current state matches requested state of master or backup
#  - check that the VRRP IP address state is correct based on our state
#  - run a custom script to check any custom block device/vip state
#  - check that the runit service status is correct based on our state
#

add_command_under_category "ha-status", "high-availability", "Show the status of high availability services.", 1 do
  if running_config.nil?
    puts "[ERROR] cannot check HA status if you haven't completed a reconfigure"
    exit 1
  end

  error_exit = 0

  if running_config['private_chef']['keepalived']['enable']
    puts "[OK] keepalived HA services enabled."
  else
    puts "[ERROR] keepalived HA services not enabled, please configure keepalived according to documentation."
    exit 1
  end


  requested_cluster_status = File.read("/var/opt/opscode/keepalived/requested_cluster_status").chomp
  current_cluster_status = File.read("/var/opt/opscode/keepalived/current_cluster_status").chomp

  if requested_cluster_status != current_cluster_status
    puts "[WARN] inconsistent cluster status, requested: #{requested_cluster_status}, current: #{current_cluster_status}, failover running?"
    error_exit = 2
  else
    puts "[OK] cluster status = #{current_cluster_status}"
  end

  vrrp_instance_ipaddress = running_config['private_chef']['keepalived']['vrrp_instance_ipaddress']
  vrrp_instance_ipaddress_dev = running_config['private_chef']['keepalived']['vrrp_instance_ipaddress_dev']
  has_vrrp_addr = `ip a show dev #{vrrp_instance_ipaddress_dev}` =~ /inet6? #{Regexp.escape(vrrp_instance_ipaddress)}/

  if has_vrrp_addr
    if current_cluster_status == "master"
      puts "[OK] found VIP IP address and I am master"
    else
      puts "[ERROR] found VRRP address and I am not master"
      error_exit = 3
    end
  else
    if current_cluster_status == "master"
      puts "[ERROR] did not find VIP IP address and I am master"
      error_exit = 3
    else
      puts "[OK] did not find VIP IP address and I am not master"
    end
  end

  vrrp_instance_interface = running_config['private_chef']['keepalived']['vrrp_instance_interface']
  has_vrrp_instance_interface = system "ip a show dev #{vrrp_instance_interface} >/dev/null 2>&1"

  if has_vrrp_instance_interface
    puts "[OK] found VRRP communications interface #{vrrp_instance_interface}"
  else
    puts "[ERROR] VRRP communications interface #{vrrp_instance_interface} not found."
    error_exit = 8
  end

  custom_backend = "/var/opt/opscode/keepalived/bin/custom_backend_"
  ['storage', 'ip'].each do |t|
    if File.executable?("#{custom_backend}#{t}")
      if system("#{custom_backend}#{t} status #{current_cluster_status}")
        puts "[OK] #{t} backend status is OK"
      else
        puts "[ERROR] #{t} backend status is not OK"
        error_exit = 4
      end
    end
  end

  # HA services are those that have a 'keepalive_me' file in their
  # runit directories
  ha_services = Dir.chdir(running_config['runit']['sv_dir']){Dir.glob('**/keepalive_me').map{|f| File.dirname(f)}}.sort

  get_all_services.sort.each do |service_name|
    # opscode-chef-mover service is only used during an upgrade, and does not need to be running all of the time
    # do not consider any hidden services when checking ha status
    next if hidden_services.include?(service_name)
    
    if service_enabled?(service_name)
      status = run_command("/opt/opscode/init/#{service_name} status >/dev/null 2>&1")
      if ha_services.include?(service_name)
        if current_cluster_status == "master"
          if status.success?
            puts "[OK] #{service_name} is running correctly, and I am master."
          else
            puts "[ERROR] #{service_name} is not running correctly, and I am master. "
            error_exit = 7
          end
        else
          if status.success?
            puts "[ERROR] #{service_name} is running, and I am not master."
            error_exit = 7
          else
            puts "[OK] #{service_name} is not running, and I am not master."
          end
        end
      else
        if status.success?
          puts "[OK] #{service_name} is running."
        else
          puts "[ERROR] #{service_name} is not running."
          error_exit = 7
        end
      end
    end
  end

  if error_exit != 0
    puts "\n[ERROR] ERRORS WERE DETECTED.\n\n"
  else
    puts "\n[OK] all checks passed.\n\n"
  end

  exit error_exit
end
