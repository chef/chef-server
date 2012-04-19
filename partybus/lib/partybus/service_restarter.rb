class Partybus::ServiceRestarter

  def restart_service(service_name)

    if ["runit", "keepalived"].include?(service_name)
      raise "Sorry yo, can't to that yet"
    end

    puts system("private-chef-ctl #{service_name} status")
    exit_status = $?.exitstatus

    unless exit_status == 3 # service is already down
      puts system("private-chef-ctl #{service_name} restart")
    end
  end

end
