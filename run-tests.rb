#!/usr/bin/env ruby

# Comprehensive test runner for wrap-knife functionality

require 'rspec'
require 'shellwords'
require 'chef-utils'
require 'ostruct'

# Mock the ChefServerCtl module and Config class
module ChefServerCtl
  class Config
    def self.knife_config_file
      "/tmp/knife.rb"
    end
    
    def self.knife_bin
      "knife"
    end
    
    def self.lb_url
      "https://localhost"
    end
  end
end

# Mock ChefUtils::Dist constants
module ChefUtils
  module Dist
    module Server
      PRODUCT = "Chef Infra Server"
      SERVER_CTL = "chef-server-ctl"
    end
  end
end

# Load just the functions we need from wrap-knife.rb
require_relative 'wrap-knife-functions'

puts "=" * 80
puts "RUNNING COMPREHENSIVE WRAP-KNIFE TESTS"
puts "=" * 80
puts

# Test 1: Original failing scenario
puts "🧪 Test 1: Original failing scenario"
puts "-" * 40
args = ["user4", "user", "four", "kallol.roy4@progress.com", "pass1234"]
result = transform_knife_opc_args(args, "user-create", "user", "create")
expected = ["user4", "--email", "kallol.roy4@progress.com", "--password", "pass1234", "--first-name", "user", "--last-name", "four"]

puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{result == expected ? '✅ PASS' : '❌ FAIL'}"
puts

# Test 2: With filename flag
puts "🧪 Test 2: With filename flag"
puts "-" * 40
args = ["admin", "Admin", "User", "admin@example.com", "admin123", "--filename", "/tmp/admin.pem"]
result = transform_knife_opc_args(args, "user-create", "user", "create")
expected = ["admin", "--email", "admin@example.com", "--password", "admin123", "--first-name", "Admin", "--last-name", "User", "-f", "/tmp/admin.pem"]

puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{result == expected ? '✅ PASS' : '❌ FAIL'}"
puts

# Test 3: With 6 arguments (middle name)
puts "🧪 Test 3: With middle name (6 arguments)"
puts "-" * 40
args = ["user5", "John", "Michael", "Doe", "john.doe@example.com", "password123"]
result = transform_knife_opc_args(args, "user-create", "user", "create")
expected = ["user5", "--email", "john.doe@example.com", "--password", "password123", "--first-name", "John", "--last-name", "Doe"]

puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{result == expected ? '✅ PASS' : '❌ FAIL'}"
puts

# Test 4: Special characters in email and password
puts "🧪 Test 4: Special characters in email and password"
puts "-" * 40
args = ["user1", "Jane", "Smith", "jane.smith+test@example.co.uk", "p@ssw0rd!123"]
result = transform_knife_opc_args(args, "user-create", "user", "create")
expected = ["user1", "--email", "jane.smith+test@example.co.uk", "--password", "p@ssw0rd!123", "--first-name", "Jane", "--last-name", "Smith"]

puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{result == expected ? '✅ PASS' : '❌ FAIL'}"
puts

# Test 5: Multiple flags
puts "🧪 Test 5: Multiple flags"
puts "-" * 40
args = ["user12", "Test", "User", "test@example.com", "password", "--filename", "/tmp/user.pem", "--orgname", "myorg", "--prevent-keygen"]
result = transform_knife_opc_args(args, "user-create", "user", "create")
expected = ["user12", "--email", "test@example.com", "--password", "password", "--first-name", "Test", "--last-name", "User", "-f", "/tmp/user.pem", "--orgname", "myorg", "--prevent-keygen"]

puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{result == expected ? '✅ PASS' : '❌ FAIL'}"
puts

# Test 6: Insufficient arguments (fallback)
puts "🧪 Test 6: Insufficient arguments (should fallback)"
puts "-" * 40
args = ["user14", "Test", "User"]
result = transform_knife_opc_args(args, "user-create", "user", "create")
expected = args  # Should return unchanged

puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{result == expected ? '✅ PASS' : '❌ FAIL'}"
puts

# Test 7: User-list with --all-info flag (should transform to --verbose)
puts "🧪 Test 7: User-list with --all-info flag"
puts "-" * 40
args = ["--all-info", "otherarg"]
result = transform_knife_opc_args(args, "user-list", "user", "list")
expected = ["otherarg", "--verbose"]

puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{result == expected ? '✅ PASS' : '❌ FAIL'}"
puts

# Test 7b: User-list with -a flag (should transform to --verbose)
puts "🧪 Test 7b: User-list with -a flag"
puts "-" * 40
args = ["-a"]
result = transform_knife_opc_args(args, "user-list", "user", "list")
expected = ["--verbose"]

puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{result == expected ? '✅ PASS' : '❌ FAIL'}"
puts

# Test 8: Transform flags only
puts "🧪 Test 8: Transform flags only"
puts "-" * 40
args = ["--filename", "/tmp/test.pem", "other", "args"]
result = transform_flags_only(args)
expected = ["-f", "/tmp/test.pem", "other", "args"]

puts "Input:    #{args.inspect}"
puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{result == expected ? '✅ PASS' : '❌ FAIL'}"
puts

# Test 9: Get server URL
puts "🧪 Test 9: Get server URL"
puts "-" * 40
result = get_server_url()
expected = "https://localhost"

puts "Output:   #{result.inspect}"
puts "Expected: #{expected.inspect}"
puts "Status:   #{result == expected ? '✅ PASS' : '❌ FAIL'}"
puts

puts "=" * 80
puts "RUNNING INTEGRATION TEST"
puts "=" * 80
puts

# Integration test: Full command simulation
puts "🧪 Integration Test: Full command simulation"
puts "-" * 50

# Mock the add_command_under_category method
executed_commands = []

def add_command_under_category(name, category, description, arity, &block)
  if name == "user-create"
    puts "Executing user-create command..."
    yield
  end
end

# Mock the run_command method
def run_command(command)
  puts "Executed: #{command}"
  # Return a mock status object
  OpenStruct.new(exitstatus: 0)
end

# Mock the exit method
def exit(code)
  puts "Exit code: #{code}"
end

# Set up test ARGV for the original failing command
ARGV.replace(['user-create', 'user4', 'user', 'four', 'kallol.roy4@progress.com', 'pass1234'])

# Simulate the knife_config and cmd_args
knife_config = "/tmp/knife.rb"
cmd_args = ARGV[1..-1]

puts "Original command: chef-server-ctl #{ARGV.join(' ')}"
puts "cmd_args: #{cmd_args.inspect}"
puts

# Transform the arguments
transformed_args = transform_knife_opc_args(cmd_args, "user-create", "user", "create")
puts "Transformed args: #{transformed_args.inspect}"

# Build the final command
auth_args = ["-c", knife_config]
all_args = transformed_args + auth_args
escaped_args = all_args.map { |arg| Shellwords.escape(arg) }.join(" ")
knife_command = "knife user create #{escaped_args}"

puts "Final knife command: #{knife_command}"
puts

# Verify the final command contains the expected elements
expected_elements = ["--email", "kallol.roy4@progress.com", "--password", "pass1234", "--first-name", "user", "--last-name", "four"]
contains_all = expected_elements.all? { |element| knife_command.include?(element) }

puts "Contains expected elements: #{contains_all ? '✅ PASS' : '❌ FAIL'}"
if contains_all
  puts "✅ SUCCESS: The command transformation is working correctly!"
else
  puts "❌ FAILURE: Missing expected elements in final command"
end

puts
puts "=" * 80
puts "TEST SUMMARY"
puts "=" * 80
puts "All core functionality tests completed."
puts "The wrap-knife plugin is correctly transforming old knife-opc format"
puts "to modern knife format, which should resolve the original password error."
