class Partybus::ServiceManager

  def restart_service(service_name)

    if ["runit", "keepalived"].include?(service_name)
      raise "Sorry yo, can't do that yet"
    end

    system("private-chef-ctl status #{service_name}")
    exit_status = $?.exitstatus

    unless exit_status == 3 # service is already down
      system("private-chef-ctl restart #{service_name}")
    end
  end

  # calls a pcc restart with sleep time after to ensure that it is up
  def force_restart_service(service_name, sleep_time)
    system("private-chef-ctl restart #{service_name}")
    if sleep_time != 0
      log "Sleeping for #{sleep_time} seconds while service #{service_name} comes back online..."
    end
    sleep sleep_time

    if not system("private-chef-ctl status #{service_name}")
      log "Error: Failed to bring service #{service_name} back online after restart..."
      exit 1
    end
  end

  # function that will as properly as possible
  # bring up a service that is needed by an upgrade
  def start_service(service_name, sleep_time)

    system("private-chef-ctl status #{service_name}")
    exit_status = $?.exitstatus

    if exit_status == 3 # service is down
      system("private-chef-ctl start #{service_name}")
      # mildly hacky, but pcc restart and status can't tell if the service is
      # actually ready to accept requests, but only if its up, sleep to ensure
      # service has time to actually be ready to accept requests
      if sleep_time != 0
        log "Sleeping for #{sleep_time} seconds while service #{service_name} comes online..."
      end
      sleep sleep_time

      if not system("private-chef-ctl status #{service_name}")
        log "Error: Failed to bring service #{service_name} back online..."
        exit 1
      end
    end
  end

  # handles list of services, but does same as the above,
  # except only sleeping at the very end
  def start_services(service_array, sleep_time)
    service_array.each do |service_name|
      # just do one sleep at the end
      start_service(service_name, 0)
    end
    if sleep_time != 0
     log "Sleeping for #{sleep_time} seconds while services comes online..."
    end
    sleep sleep_time
  end

  def stop_service(service_name, sleep_time)
    system("private-chef-ctl stop #{service_name}")

    # if the service didn't go down, gracefully kill it
    if system("private-chef-ctl status #{service_name}")
      log "Error: service #{service_name} failed to stop, killing gracefully..."
      system("private-chef-ctl graceful-kill #{service_name}")
    end
    # give the systems a few seconds to bring things down
    if sleep_time != 0
      log "Sleeping for #{sleep_time} seconds while service #{service_name} stops..."
    end
    sleep sleep_time
    # throw an error if the service did not stop
    if system("private-chef-ctl status #{service_name}")
      log "Failure: service #{service_name} could not be stopped or killed."
      exit 1
    end
  end

  def stop_services(service_array, sleep_time)
    service_array.each do |service_name|
      # just do one sleep at the end
      stop_service(service_name, 0)
    end
    if sleep_time != 0
     log "Sleeping for #{sleep_time} seconds while services stop..."
    end
    sleep sleep_time
  end

end
