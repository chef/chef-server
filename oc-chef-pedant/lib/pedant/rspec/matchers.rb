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

require 'rspec/expectations'

module RSpec
  module Matchers

    def strictly_match(expected)
      PedantHashComparator.new(expected, :strict)
    end

    def loosely_match(expected)
      PedantHashComparator.new(expected, :loose)
    end

    class PedantHashComparator
      def initialize(expected, mode=:strict)
        @expected = expected
        @mode = mode
      end

      attr_reader :expected

      def strict?
        @mode == :strict
      end

      def matches?(actual)
        return false unless actual.class == @expected.class or (actual.class == String and @expected.class == Regexp)
        if actual.is_a? Hash
          hash_matches?(actual)
        elsif actual.is_a? Array
          @expected = {:hashwrapper => @expected}
          result = hash_matches?({ :hashwrapper => actual } )
          @expected = @expected[:hashwrapper]
          result
        else
          # Using this, we can support matching regexp array elements
          if @expected.class == Regexp
            @expected =~ actual
          else
            @expected == actual
          end
        end
      end # matches?

      def hash_matches?(actual)
        @actual = actual

        if strict?
          keys_match = (expected.keys.sort == actual.keys.sort)
          # if they keys don't match, we can short-circuit here
          return false unless keys_match
        end

        @expected.keys.all? do |key|
          spec = @expected[key]
          value = actual[key]

          case spec
          when Regexp
            spec =~ value
          when Array
            array_matches?(spec, value)
          when Hash
            PedantHashComparator.new(spec, @mode).matches?(value)
          when Proc then
            spec.call(value)
          else
            spec == value
          end
        end

      end

      def array_matches?(spec, value)
        # we care about contents, not order; i.e., treat them
        # (kind of) like sets
        return true if spec == value
        return false if value.length < spec.length
        return false if strict? and value.length != spec.length
        # Note this is going to be slow for large arrays,
        # though our actual usage shows we don't have any arrays on a
        # scale that would matter.
        return true if spec.all? { |s| value.include? s }
        # Our comparisons are more complex than simple value
        spec.each_with_index  do |newspec, x|
          return false unless PedantHashComparator.new(newspec, @mode).matches? value[x]
        end
        return true
      end

      def friendly_actual
        if @actual.class == Hash and @actual.has_key? :hashwrapper
          @actual[:hashwrapper]
        else
          @actual
        end
      end

      def description
        if strict?
          "respond with all keys matching"
        else
          "respond with all specified keys matching"
        end
      end

      def failure_message
        """
Expected a #{strict? ? "full" : "partial"} match of the result

  #{PP.pp(friendly_actual, "")}

to the spec

  #{PP.pp(@expected, "")}

to succeed, but it didn't!
"""
      end

      def failure_message_when_negated
        """
Expected a #{strict? ? "full" : "partial"} match of the result

  #{PP.pp(friendly_actual, "")}

to the spec

  #{PP.pp(@expected, "")}

to fail, but it succeeded!
"""
      end




    end
  end
end # PedantHashComparator




RSpec::Matchers.define :have_status_code do |code|
  match do |response|
    if code.respond_to?(:any?)
      code.any?{|c| response.code == c }
    else
      response.code == code
    end
  end

  codes = Pedant::RSpec::HTTP::STATUS_CODES

  description do
    "respond with #{code} #{codes[code]}"
  end

  failure_message do |response|
    parsed = parse(response)
    message = "Response should have HTTP status code #{code} ('#{codes[code]}'), but it was actually #{response.code} ('#{codes[response.code]}')"
    message << "\n  Reponse Body: #{response}" if parsed
    message
  end
end

RSpec::Matchers.define :have_error_message do |message|
  match do |response|
    Pedant.config.verify_error_messages || parse(response)["error"] == message
  end

  description do
    "respond with error message '#{message}'"
  end
end

RSpec::Matchers.define :have_error do |code, message|
  match do |response|
    response.code == code && (Pedant.config.verify_error_messages || parse(response) == { "error" => [message] })
  end

  codes = Pedant::RSpec::HTTP::STATUS_CODES

  description do
    "respond with #{code} #{codes[code]} and an error message of '#{message}'"
  end

  failure_message do |response|
    <<-EOM
1) HTTP status code should have been #{code} ('#{codes[code]}'); it was #{response.code}.

2) The response should have contained the error message

       #{message}

   Instead, the entire response body was

       #{response}

EOM
  end
end

# Test various aspects of an HTTP response.  The response will be
# compared to an "expected response spec", which is a hash that
# describes the various tests that should be run.  Currently a
# response spec can have the following keys (all are optional):
#
# :status => Value is the integer HTTP status code the response should
#     have
#
# :body => A map of string keys to expected values that a JSON-encoded
#     body should have.  Only the keys specified are matched; the test
#     makes no assumptions about keys that are not listed.  A value
#     can be a string or number literal, or a regular expression (which
#     the expected value should match).
#
# :body_exact => same as :body, but all keys are expected to be
#     present.  That is, if there are keys in the body that you do not
#     specify in your test, the test will fail.
#
# :body_raw => does no JSON parsing of the body, and instead does a
#     raw string match
#
# :headers => a map of header names and expected values.  Only tests
#     the specified headers, and makes no assumptions about
#     unspecified headers.
# :no_headers => an array of header names that should NOT be present

RSpec::Matchers.define :look_like do |expected_response_spec|
  include ::Pedant::JSON

  match do |response|
    begin
      things_to_check = expected_response_spec.keys
      json_tests = [:body, :body_exact]

      # Test the HTTP Status Code, if given
      if expected_response_spec[:status]
        expect(response).to have_status_code expected_response_spec[:status]
      end

      # If you want to check the raw, unprocessed body for some
      # reason.  Mainly useful for asserting a response has a
      # completely empty body.
      if expected_response_spec[:body_raw]
        expect(response).to eq expected_response_spec[:body_raw]
      end

      actual_headers = response.raw_headers
      # Test the headers
      if expected_response_spec[:headers]
        headers = expected_response_spec[:headers]
        headers.each do |header, value|
          actual_headers[header].should eq value
        end
      end

      # Test headers that shouldn't be
      if expected_response_spec[:no_headers]
        expected_response_spec[:no_headers].each do |header|
          actual_headers.key?(header).should be(false)
        end
      end

      if not (things_to_check & json_tests).empty? # '&' = intersection
        is_error = expected_response_spec[:status] && expected_response_spec[:status] >= 400
        if Pedant.config.verify_error_messages || !is_error
          # Only parse the body as JSON if we're going to test it as
          # JSON.  While all the "normal" calls to the API should return
          # non-empty JSON bodies, some calls may not (such as trying to
          # use a non-allowed HTTP method and getting a 405 response
          # back with an empty body).  In cases like that, trying to
          # parse an empty body will result in an error.
          parsed_json = parse(response)

          expected_body_spec = expected_response_spec[:body] || expected_response_spec[:body_exact]

          # :body_exact implies that there should be no keys that are
          # untested, i.e., you test everything that's there. It does not
          # imply ordering.
          if expected_body_spec.is_a?(Hash) or expected_body_spec.is_a?(Array)
            if expected_response_spec[:body_exact]
              expect(parsed_json).to strictly_match expected_body_spec
            else # just a body spec (looser)
              expect(parsed_json).to loosely_match expected_body_spec
            end
          else
            expect(parsed_json).to eql(expected_body_spec)
          end
        end
      end

      true
    rescue RSpec::Expectations::ExpectationNotMetError => e
      @error_message = e.message
      # fail the overall matcher
      false
    end
  end

  description do
    code = expected_response_spec[:status]
    "respond with #{code} #{Pedant::RSpec::HTTP::STATUS_CODES[code]}"
  end

  failure_message do |response|
    @error_message
  end
end


# Knife Matchers

# 'outcome_spec' is a hash with any of the keys :status, :stdout, or
# :stderr.  The value of :status, if given, should be the integer exit
# status of the executed command.  :stdout and :stderr, if given, are
# regular expressions that should match the respective stream's
# output.

RSpec::Matchers.define :have_outcome do |outcome_spec|
  match do |executed_shellout_command|
    valid_keys =  [:status, :stdout, :stderr]
    if outcome_spec.keys & valid_keys == []
      throw "You did not specify values for any of #{valid_keys}!"
    end

    status = outcome_spec[:status] ? (executed_shellout_command.exitstatus == outcome_spec[:status]) : true
    is_error = outcome_spec[:status] && outcome_spec[:status] != 0
    if outcome_spec[:stdout] && (Pedant.config.verify_error_messages || !is_error)
      stdout = executed_shellout_command.stdout =~ outcome_spec[:stdout]
    else
      stdout = true
    end
    if outcome_spec[:stderr] && (Pedant.config.verify_error_messages || !is_error)
      stderr = executed_shellout_command.stderr =~ outcome_spec[:stderr]
    else
      stderr = true
    end
    status && stdout && stderr
  end

  # Could just spit out `executed_shellout_command.inspect`, but I
  # find the formatting suboptimal for testing error messages.
  failure_message do |executed_shellout_command|
    "Executed command should have matched the outcome spec #{outcome_spec.inspect}, but it didn't!\n
\tFailed Command: #{executed_shellout_command.command}\n
\tCommand Setting: #{Pedant::Knife.command_setting(executed_shellout_command).inspect}\n
\tExit Status: #{executed_shellout_command.exitstatus}\n
\tStandard Output:\n
#{executed_shellout_command.stdout}\n
\tStandard Error:\n
#{executed_shellout_command.stderr}"
  end

end
