#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

add_command "master-recover", "Set this server to HA master state, ignoring VRRP.", 1 do
  exec "/var/opt/opscode/keepalived/bin/cluster.sh master"
end

add_command "backup-recover", "Set this server to HA backup state, ignoring VRRP", 1 do
  exec "/var/opt/opscode/keepalived/bin/cluster.sh backup"
end

#
# ha-status
#  - check keepalived is actually enabled in the config
#  - check drbd is actually enabled in the config
#  - check that someone created the underlying block device drbd will use
#  - check that the drbd device itself exists (i.e. kernel module configured+loaded)
#  - check that the current state matches requested state of master or backup
#  - check that the VRRP IP address state is correct based on our state
#  - check that the DRBD state is correct based on our state
#  - check that the DRBD drive is correctly (un)mounted based on our state
#  - check that the DRBD primary+secondary replication IP are both pingable
#  - check that the runit service status is correct based on our state
#

add_command "ha-status", "Show the status of high availability services.", 1 do
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

  if running_config['private_chef']['drbd']['enable']
    puts "[OK] DRBD disk replication enabled."
  else
    puts "[ERROR] DRBD disk replication services not enabled, please configure DRBD according to documentation."
    exit 1
  end

  drbd_disk = running_config['private_chef']['drbd']['disk']
  if File.exists?(drbd_disk)
    puts "[OK] DRBD partition #{drbd_disk} found."
  else
    puts "[ERROR] DRBD partition #{drbd_disk} not found, please FIXME."
    exit 1
  end

  drbd_device = running_config['private_chef']['drbd']['device']
  if File.exists?(drbd_device)
    puts "[OK] DRBD device #{drbd_device} found."
  else
    puts "[ERROR] DRBD device #{drbd_device} not found, please FIXME."
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
  has_vrrp_addr = `ip a show dev #{vrrp_instance_ipaddress_dev}` =~ /#{vrrp_instance_ipaddress}/

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

  drbd_status = `cat /proc/drbd | egrep cs:`.chomp
  if current_cluster_status == "master"
    if drbd_status =~ /cs:Connected ro:Primary\/Secondary ds:UpToDate\/UpToDate/
      puts "[OK] my DRBD status is Connected/Primary/UpToDate and I am master"
    else
      puts "[ERROR] my DRBD status is: #{drbd_status}"
      error_exit = 4
    end
  else
    if drbd_status =~ /cs:Connected ro:Secondary\/Primary ds:UpToDate\/UpToDate/
      puts "[OK] my DRBD status is Connected/Secondary/UpToDate and I am not master"
    else
      puts "[ERROR] my DRBD status is #{drbd_status}"
      error_exit = 4
    end
  end

  drbd_mountpoint = running_config['private_chef']['drbd']['data_dir']
  mps = []
  File.read("/proc/mounts").each_line { |l| mps << l.split(/\s+/)[1] }
  if current_cluster_status == "master"
    if mps.include?(drbd_mountpoint)
      puts "[OK] my DRBD partition is mounted and I am master"
    else
      puts "[ERROR] my DRBD partition is not mounted and I am master"
      error_exit = 5
    end
  else
    if mps.include?(drbd_mountpoint)
      puts "[ERROR] my DRBD partition is mounted and I am not master"
      error_exit = 5
    else
      puts "[OK] my DRBD partition is not mounted and I am not master"
    end
  end

  drbd_primary_ip = running_config['private_chef']['drbd']['primary']['ip']
  if system("ping -c 1 -W 1 #{drbd_primary_ip} >/dev/null 2>&1")
    puts "[OK] DRBD primary IP address pings"
  else
    puts "[ERROR] DRBD primary IP address does not ping"
    error_exit = 6
  end

  drbd_secondary_ip = running_config['private_chef']['drbd']['secondary']['ip']
  if system("ping -c 1 -W 1 #{drbd_secondary_ip} >/dev/null 2>&1")
    puts "[OK] DRBD secondary IP address pings"
  else
    puts "[ERROR] DRBD secondary IP address does not ping"
    error_exit = 6
  end

  # HA services are those that have a 'keepalive_me' file in their
  # runit directories
  ha_services = Dir.chdir(running_config['runit']['sv_dir']){Dir.glob('**/keepalive_me').map{|f| File.dirname(f)}}.sort

  get_all_services.sort.each do |service_name|
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
