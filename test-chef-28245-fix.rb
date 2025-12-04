#!/usr/bin/env ruby
#
# Standalone test for CHEF-28245 fix - wrap-knife user-list flag transformation
#

require "mixlib/cli"

# Mock ChefUtils::Dist constants
module ChefUtils
  module Dist
    module Server
      PRODUCT = "Chef Infra Server"
      SERVER_CTL = "chef-server-ctl"
    end
  end
end

# Argument parser using Mixlib::CLI to separate flags from positional args
class KnifeArgumentParser
  include Mixlib::CLI
  
  option :file,
    short: "-f FILE",
    long: "--file FILE",
    description: "Write the private key to a file"
    
  option :filename,
    long: "--filename FILE", 
    description: "Write the private key to a file (knife-opc compatibility)"
    
  option :user_key,
    long: "--user-key FILENAME",
    description: "Set the initial default key for the user from a file"
    
  option :prevent_keygen,
    short: "-k",
    long: "--prevent-keygen",
    description: "Prevent server from generating a default key pair",
    boolean: true
    
  option :orgname,
    long: "--orgname ORGNAME",
    short: "-o ORGNAME", 
    description: "Associate new user to an organization"
    
  option :passwordprompt,
    long: "--prompt-for-password",
    short: "-p",
    description: "Prompt for user password",
    boolean: true
    
  option :first_name,
    long: "--first-name FIRST_NAME",
    description: "First name for the user"
    
  option :last_name,
    long: "--last-name LAST_NAME", 
    description: "Last name for the user"
    
  option :email,
    long: "--email EMAIL",
    description: "Email for the user"
    
  option :password,
    long: "--password PASSWORD",
    description: "Password for the user"
    
  def self.parse_args(args)
    parser = new
    name_args = parser.parse_options(args.dup)
    { positional: name_args, config: parser.config }
  end
end

# Transform arguments from knife-opc format to native knife format
def transform_knife_opc_args(args, chef_server_ctl_cmd, _knife_noun, _knife_verb)
  transformed = args.dup
  
  case chef_server_ctl_cmd
  when "user-create"
    parsed = KnifeArgumentParser.parse_args(args)
    positional_args = parsed[:positional]
    config = parsed[:config]
    
    if positional_args.length >= 5
      username = positional_args[0]
      first_name = nil
      last_name = nil
      email = nil
      password = nil
      
      if positional_args.length == 5
        first_name = positional_args[1]
        last_name = positional_args[2]
        email = positional_args[3]
        password = positional_args[4]
      elsif positional_args.length == 6
        first_name = positional_args[1]
        last_name = positional_args[3]    
        email = positional_args[4]
        password = positional_args[5]
      else
        return transform_flags_only(args)
      end
      
      transformed = [username]
      transformed << "--email" << email
      transformed << "--password" << password
      transformed << "--first-name" << first_name
      transformed << "--last-name" << last_name
      
      config.each do |key, value|
        case key
        when :filename
          transformed << "-f" << value if value
        when :file
          transformed << "-f" << value if value
        when :orgname
          transformed << "--orgname" << value if value
        when :user_key
          transformed << "--user-key" << value if value
        when :prevent_keygen
          transformed << "--prevent-keygen" if value
        when :passwordprompt
          transformed << "--prompt-for-password" if value
        end
      end
    else
      transformed = transform_flags_only(args)
    end
    
  when "user-list"
    # Transform knife-opc --all-info/-a flag to native knife --verbose flag
    if transformed.include?("--all-info") || transformed.include?("-a")
      transformed = transformed.reject { |arg| %w[--all-info -a].include?(arg) }
      transformed << "--verbose"
    end
  end
  
  transformed
end

# Transform flags only (for non-opc format args) 
def transform_flags_only(args)
  args.map do |arg|
    case arg
    when "--filename"
      "-f"
    else
      arg
    end
  end
end

# ============================================================================
# TESTS START HERE
# ============================================================================

puts "=" * 80
puts "TESTING WRAP-KNIFE USER-LIST FLAG TRANSFORMATION (CHEF-28245 FIX)"
puts "=" * 80
puts

test_passed = 0
test_failed = 0

# Test 1: --all-info flag
puts "Test 1: --all-info flag should transform to --verbose"
puts "-" * 40
args = ["--all-info"]
result = transform_knife_opc_args(args, "user-list", "user", "list")
expected = ["--verbose"]
passed = result == expected
status = passed ? "✅ PASS" : "❌ FAIL"
passed ? test_passed += 1 : test_failed += 1
puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{status}"
puts

# Test 2: -a flag
puts "Test 2: -a flag should transform to --verbose"
puts "-" * 40
args = ["-a"]
result = transform_knife_opc_args(args, "user-list", "user", "list")
expected = ["--verbose"]
passed = result == expected
status = passed ? "✅ PASS" : "❌ FAIL"
passed ? test_passed += 1 : test_failed += 1
puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{status}"
puts

# Test 3: -a with other args
puts "Test 3: -a flag with --with-uri should add --verbose"
puts "-" * 40
args = ["-a", "--with-uri"]
result = transform_knife_opc_args(args, "user-list", "user", "list")
expected = ["--with-uri", "--verbose"]
passed = result == expected
status = passed ? "✅ PASS" : "❌ FAIL"
passed ? test_passed += 1 : test_failed += 1
puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{status}"
puts

# Test 4: --all-info with other args
puts "Test 4: --all-info with --with-uri should add --verbose"
puts "-" * 40
args = ["--all-info", "--with-uri"]
result = transform_knife_opc_args(args, "user-list", "user", "list")
expected = ["--with-uri", "--verbose"]
passed = result == expected
status = passed ? "✅ PASS" : "❌ FAIL"
passed ? test_passed += 1 : test_failed += 1
puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{status}"
puts

# Test 5: No flags
puts "Test 5: user-list without -a or --all-info should not add --verbose"
puts "-" * 40
args = []
result = transform_knife_opc_args(args, "user-list", "user", "list")
expected = []
passed = result == expected
status = passed ? "✅ PASS" : "❌ FAIL"
passed ? test_passed += 1 : test_failed += 1
puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{status}"
puts

# Test 6: Only --with-uri
puts "Test 6: user-list with only --with-uri should not add --verbose"
puts "-" * 40
args = ["--with-uri"]
result = transform_knife_opc_args(args, "user-list", "user", "list")
expected = ["--with-uri"]
passed = result == expected
status = passed ? "✅ PASS" : "❌ FAIL"
passed ? test_passed += 1 : test_failed += 1
puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{status}"
puts

puts "=" * 80
puts "TEST SUMMARY FOR CHEF-28245 FIX"
puts "=" * 80
puts "Total:  #{test_passed + test_failed}"
puts "Passed: #{test_passed}"
puts "Failed: #{test_failed}"
puts "=" * 80

if test_failed == 0
  puts "✅ ALL TESTS PASSED - Fix verified!"
  puts ""
  puts "Summary: The fix correctly transforms the knife-opc -a/--all-info flags"
  puts "to the native knife --verbose flag, maintaining backward compatibility"
  puts "for users while adapting to the knife-opc removal."
  exit 0
else
  puts "❌ SOME TESTS FAILED - Fix needs attention"
  exit 1
end
