#
# Copyright:: Copyright (c) 2017 Chef Software, Inc.
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

require_relative './preflight_checks.rb'

class Ipv6PreflightValidator < PreflightValidator
  def initialize(node)
    super
  end

  def run!
    verify_ipv6_for_lo
  end

  # When setting the environment variable ERL_EPMD_ADDRESS to _anything_, epmd
  # will _also_ try to listen on 127.0.0.1 _and_ ::1:
  # https://github.com/erlang/otp/blob/47e46069d019/erts/epmd/src/epmd_srv.c#L255-L261
  #
  # We set ERL_EPMD_ADDRESS=127.0.0.1 in each of our Erlang services' run files.
  #
  # If IPv6 is disabled completely, epmd's try to bind to ::1 fails with an
  # error that is ignored:
  # https://github.com/erlang/otp/blob/47e46069d019/erts/epmd/src/epmd_srv.c#L370-L372
  #
  # If IPv6 is not disabled completely, that bind fails more harshly, and causes
  # epmd to not come up, making the service's bootup fail, too, as it cannot
  # connect to epmd.
  #
  # See also http://erlang.org/pipermail/erlang-questions/2016-March/088427.html
  def verify_ipv6_for_lo
    if ipv6_enabled && !lo_has_ipv6_addr
      fail_with <<EOF
Your system has IPv6 enabled but its loopback interface has no IPv6
address.

You must either pass `ipv6.disable=1` to your kernel command line,
to completely disable IPv6, or ensure the loopback interface has an
`::1` address by running

    sysctl net.ipv6.conf.lo.disable_ipv6=0
EOF
    end
  end

  def ipv6_enabled
    # if kernel cmdline has ipv6.disable=1, this doesn't exist
    ::File.exists?("/proc/sys/net/ipv6")
  end

  def lo_has_ipv6_addr
    IO.read("/proc/sys/net/ipv6/conf/lo/disable_ipv6").chomp == "0"
  end
end
