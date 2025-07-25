#!/usr/bin/env ruby

# Test script for knife-opc argument transformation

# Load the transform function from the wrapper
require_relative 'plugins/wrap-knife-opc'

def test_user_create_transformation
  puts "Testing user-create argument transformation:"
  puts "=" * 50
  
  # Test case 1: Standard knife-opc format
  knife_opc_args = ["janedoe", "Jane Doe", "Jane", "Doe", "janed@example.com", "abc123", "--filename", "/tmp/janedoe.pem"]
  
  puts "Input (knife-opc format):"
  puts "  #{knife_opc_args.join(' ')}"
  
  transformed = transform_knife_opc_args(knife_opc_args, "user-create", "user", "create")
  
  puts "\nOutput (native knife format):"
  puts "  #{transformed.join(' ')}"
  
  puts "\nExpected transformation:"
  puts "  janedoe --email janed@example.com --password abc123 --first-name Jane --last-name Doe --file /tmp/janedoe.pem"
  
  # Test case 2: With --filename= format
  puts "\n" + "=" * 50
  knife_opc_args2 = ["johndoe", "John Doe", "John", "Doe", "johnd@example.com", "xyz789", "--filename=/tmp/johndoe.pem"]
  
  puts "Input (knife-opc format with --filename=):"
  puts "  #{knife_opc_args2.join(' ')}"
  
  transformed2 = transform_knife_opc_args(knife_opc_args2, "user-create", "user", "create")
  
  puts "\nOutput (native knife format):"
  puts "  #{transformed2.join(' ')}"
  
  puts "\nExpected transformation:"
  puts "  johndoe --email johnd@example.com --password xyz789 --first-name John --last-name Doe --file=/tmp/johndoe.pem"
end

def test_user_list_transformation
  puts "\n\nTesting user-list argument transformation:"
  puts "=" * 50
  
  # Test case: user-list with --all-info (should be removed)
  knife_opc_args = ["--all-info", "-w"]
  
  puts "Input (knife-opc format):"
  puts "  #{knife_opc_args.join(' ')}"
  
  transformed = transform_knife_opc_args(knife_opc_args, "user-list", "user", "list")
  
  puts "\nOutput (native knife format):"
  puts "  #{transformed.join(' ')}"
  
  puts "\nExpected: --all-info should be removed, -w should remain"
end

def test_command_construction
  puts "\n\nTesting full command construction:"
  puts "=" * 50
  
  # Simulate what the wrapper would generate
  knife_cmd = "/opt/opscode/embedded/bin/knife"
  knife_config = "/tmp/knife.rb"
  
  args = ["janedoe", "Jane Doe", "Jane", "Doe", "janed@example.com", "abc123", "--filename", "/tmp/janedoe.pem"]
  transformed = transform_knife_opc_args(args, "user-create", "user", "create")
  escaped_args = transformed.map { |a| require 'shellwords'; Shellwords.escape(a) }.join(" ")
  
  full_command = "#{knife_cmd} user create #{escaped_args} -c #{knife_config}"
  
  puts "Full command that would be executed:"
  puts "  #{full_command}"
end

# Run the tests
test_user_create_transformation
test_user_list_transformation
test_command_construction

puts "\n" + "=" * 50
puts "Test completed!"
