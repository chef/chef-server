# Copyright: Copyright (c) 2012 Opscode, Inc.
# License: Apache License, Version 2.0
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

require 'pedant/rspec/http_status_codes'

RSpec::Matchers.define :be_create_response_for do |type|
  match do |response|

    parsed = parse(response)
    id = parsed["id"]

    # The server is hard coded in the opscode-authz application... We'll warn about it for the time being.
    puts "NOTE: URI returned from creation of a new #{type} is currently matching the server 'authz.opscode.com', which is hard coded!"
    expected_uri = "http://authz.opscode.com/#{type}s/#{id}"

    response.code == 201 && parsed.keys.sort == ["id", "uri"] && parsed["uri"] == expected_uri
  end
  description do
    "yield a suitable response for #{type} creation"
  end

  failure_message_for_should do |response|
    "Received a HTTP response with code #{response.code} (#{Pedant::RSpec::HTTP::STATUS_CODES[response.code]}) and a body of #{response}"
  end
end

RSpec::Matchers.define :be_a_direct_member_of do |group_label|
  match do |actor_or_group_label|

    unless actor_or_group_label.is_a? Symbol
      raise """
      Please call the 'be_a_direct_member_of' matcher on a Symbol.
      It allows for more informative error messages.
      """
    end

    unless group_label.is_a? Symbol
      raise """
      Please pass a Symbol to the 'be_a_direct_member_of' matcher.
      It allows for more informative error messages.
      """
    end

    @label = actor_or_group_label
    @id = resolve(actor_or_group_label)

    group = get("/groups/#{resolve(group_label)}", :superuser)

    if group.code != 200
      raise "Retrieval of group named '#{group_label}' returned an unexpected HTTP code of #{group.code}\n#{group.inspect}"
    end

    parse(group)["actors"].include?(@id) || parse(group)["groups"].include?(@id)

  end
end

RSpec::Matchers.define :directly_have_permission do |permission|
  match do |actor_or_group_label|

    unless actor_or_group_label.is_a? Symbol
      raise """
      Please call the 'directly_have_permission' matcher on a Symbol.
      It allows for more informative error messages.
      """
    end

    unless @chained
      raise """
      You must add a chained call when using the 'directly_have_permission' matcher.

      Please select from:

        on_actor
        on_container
        on_group
        on_object

      You may choose only one.
      """
    end

    @label = actor_or_group_label
    @id = resolve(actor_or_group_label)

    acl = get("/#{@type}s/#{@target}/acl", :superuser)
    if acl.code != 200
      raise "Retrieval of ACL for #{@type} identified by #{@target} returned an unexpected HTTP code of #{acl.code}\n#{acl.inspect}"
    end

    parsed = parse(acl)[permission.to_s]

    # I don't really like this... but it works for now.  An
    # alternative is to make the call to /actors/<actor_id> and
    # /groups/<group_id> to make the determination
    parsed["actors"].include?(@id) || parsed["groups"].include?(@id)
  end

  def verify_only_one_chain
    if @chained
      raise "Already chained the matcher once!"
    else
      @chained = true
    end
  end

  def chain_symbols(maybe_symbol)
    unless maybe_symbol.is_a? Symbol
      raise """
      Please pass a Symbol to the 'on_#{@type}' chain of the 'directly_have_permission' matcher.
      It allows for more informative error messages.
      """
    end
  end

  [:actor, :container, :group, :object].each do |type|
    chain "on_#{type}".to_sym do |label|
      verify_only_one_chain
      @type = type
      chain_symbols(label)
      @target = resolve(label)
      @target_name = label
    end
  end

  failure_message_for_should do |actor_or_group|
    """
    The actor or group
      #{@id} (named '#{@label}' in this test)

    should have had the '#{permission}' permission directly granted on the
      #{@type} #{@target} (named '#{@target_name}' in this test)

    but it didn't!
    """
  end

  failure_message_for_should_not do |actor_or_group|
    """
    The actor or group
      #{@id} (named '#{@label}' in this test)

    should NOT have had the '#{permission}' permission directly granted on the
      #{@type} #{@target} (named '#{@target_name}' in this test)

    but it did!
    """
  end
end

RSpec::Matchers.define :have_status_code do |code|
  match do |response|
    body_test = if @body
                  parse(response) == @body
                else
                  true
                end

    (response.code == code) && body_test

  end

  chain :with_body do |body|
    unless body.is_a? Hash
      raise "The 'with_body' chain of 'have_status_code' currently only accepts hashes"
    end
    @body = body
  end

  codes = Pedant::RSpec::HTTP::STATUS_CODES

  failure_message_for_should do |response|
    """
    Expected:
      #{code} ('#{codes[code]}')
      #{@body ? @body.inspect : ""}

    Got:
      #{response.code} ('#{codes[response.code]}')
      #{parse(response)}

      (Note: the response body is always displayed for debugging purposes,
       even if you didn't explicitly match on it.)
    """

  end
end
