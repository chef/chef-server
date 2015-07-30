#
# Copyright 2015 Chef Software, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require 'optparse'

def check_services(service_names)
  statuses = service_names.map do |svc|
    check_service(svc)
  end
  return statuses
end

def check_service(service_name)
  status = run_command("#{base_path}/init/#{service_name} status >/dev/null 2>&1")
  status.success?
end

add_command_under_category "wait", "service-management", "Wait for a given service to be up", 2 do
  wait_args = ARGV[3..-1]
  options = {}

  OptionParser.new do |opts|
    opts.on("-t SECONDS", "--timeout SECONDS", "Timeout after SECONDS seconds.") do |t|
      options[:timeout] = t.to_i
    end

    opts.on("-d", "--down", "Wait until the services are down rather than up") do |d|
      options[:down] = d
    end
  end.parse!(wait_args)

  service_names = wait_args
  start_msg = "Waiting for service#{ service_names.length > 1 ? "s" : ""}: #{service_names.join(" ")} to be "
  if options[:down]
    start_msg << "down"
  else
    start_msg << "up"
  end

  begin
    Timeout::timeout(options[:timeout]) do
      loop do
        status = check_services(service_names)
        if options[:down]
          break if status.none?
        else
          break if status.all?
        end
        sleep 0.25
      end
    end
  rescue Timeout::Error
    $stderr.puts "Timed out after #{options[:timeout]} seconds"
    exit 1
  end

  exit 0
end
