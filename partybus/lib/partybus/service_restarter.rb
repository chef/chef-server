class Partybus::ServiceRestarter

  def restart_service(service_name)

    if ["runit", "keepalived"].include?(service_name)
      raise "Sorry yo, can't to that yet"
    end

    system("private-chef-ctl status #{service_name}")
    exit_status = $?.exitstatus

    unless exit_status == 3 # service is already down
      system("private-chef-ctl restart #{service_name}")
    end
  end

end
