# Copyright: Copyright 2012-2018 Chef Software, Inc.
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

        on

      You may choose only 'on'. :)
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
      Please pass a Symbol to the 'on' chain of the 'directly_have_permission' matcher.
      It allows for more informative error messages.
      """
    end
  end

  # This doesn't have access to @thingies (it's not the same class),
  # so the type needs to be passed explicitly
  chain :on do |type, label|
    verify_only_one_chain
    @type = type
    chain_symbols(label)
    @target = resolve(label)
    @target_name = label
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

RSpec::Matchers.define :nested_verify do |a, b|
  if (b.class == Regexp)
    a.should =~ b
  elsif (b.class == Array)
    a.length.should == b.length
    sorted_a = a.sort
    sorted_b = b.sort
    sorted_b.each_index do |i|
      nested_verify(sorted_a[i], sorted_b[i])
    end
  elsif (b.class == Hash)
    a.each_key do |key|
      nested_verify(a[key], b[key])
    end
  else
    a.should == b
  end
end

RSpec::Matchers.define :have_status_code do |code|
  match do |response|
    body_test = if @body
                  begin
                    response_body = parse(response)
                    response_body.keys.sort.should == @body.keys.sort
                    response_body.each_key do |key|
                      nested_verify(response_body[key], @body[key])
                    end
                    true
                  rescue JSON::ParserError => e
                    false
                  end
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

  chain :with_info do |info|
    @info = info
  end

  codes = Pedant::RSpec::HTTP::STATUS_CODES

  failure_message_for_should do |response|
    begin
      parsed_response = parse(response)
    rescue JSON::ParserError => e
      parsed_response = "error parsing response:\n      #{e}"
      if (response.nil?)
        parsed_response += "\n      response is nil"
      else
        parsed_response += "\n      response is #{response}"
      end
    end

    if (@info)
      info_string = "\n    Additional Information:\n      #{@info}\n"
    else
      info_string = ""
    end
    """
    Expected:
      #{code} ('#{codes[code]}')
      #{@body ? @body.inspect : ""}

    Got:
      #{response.code} ('#{codes[response.code]}')
      #{parsed_response}
      #{info_string}
      (Note: the response body is always displayed for debugging purposes,
       even if you didn't explicitly match on it.)
    """

  end
end
