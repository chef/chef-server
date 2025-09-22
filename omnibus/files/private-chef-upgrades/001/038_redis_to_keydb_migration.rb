define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    
    log "Starting Redis to KeyDB migration - fixing WRONGTYPE errors for xmaint_allowed_ips_list"
    
    # Make sure redis_lb service is running for the migration
    start_services(['redis_lb'])
    sleep 10
    
    # Run the migration script using Chef Server's embedded Ruby
    migration_script = <<-RUBY_SCRIPT
require 'redis'
require 'json'

begin
  # Load Redis configuration from chef-server-running.json
  config_file = '/etc/opscode/chef-server-running.json'
  if File.exist?(config_file)
    config = JSON.parse(File.read(config_file))
    redis_config = config.dig('private_chef', 'redis_lb') || {}
    host = redis_config['bind'] || '127.0.0.1'
    port = redis_config['port'] || 16379
    password = redis_config['password']
  else
    host = '127.0.0.1'
    port = 16379
    password = nil
  end

  # Connect to Redis/KeyDB
  options = { host: host, port: port, timeout: 5 }
  options[:password] = password if password
  redis = Redis.new(options)
  
  # Test connection
  redis.ping
  puts "✓ Connected to Redis/KeyDB at \#{host}:\#{port}"
  
  # Check and fix xmaint_allowed_ips_list key
  key = 'xmaint_allowed_ips_list'
  
  if redis.exists(key) > 0
    current_type = redis.type(key)
    puts "Current type of \#{key}: \#{current_type}"
    
    if current_type != 'set'
      # Backup current value
      backup_key = "\#{key}_backup_\#{Time.now.to_i}"
      case current_type
      when 'string'
        value = redis.get(key)
        redis.set(backup_key, value)
        puts "Backed up string value to \#{backup_key}"
        
        # Parse the value to extract IPs
        ips = []
        begin
          parsed = JSON.parse(value)
          ips = parsed.is_a?(Array) ? parsed : [value]
        rescue JSON::ParserError
          ips = [value]
        end
      when 'list'
        values = redis.lrange(key, 0, -1)
        values.each { |v| redis.rpush(backup_key, v) }
        ips = values
        puts "Backed up list values to \#{backup_key}"
      when 'hash'
        hash = redis.hgetall(key)
        redis.mapped_hmset(backup_key, hash)
        ips = hash.values
        puts "Backed up hash values to \#{backup_key}"
      end
      
      # Delete the old key and create as SET
      redis.del(key)
      
      # Add IPs to the set, ensuring we have at least localhost
      ips.each do |ip|
        next if ip.nil? || ip.strip.empty?
        redis.sadd(key, ip.strip)
      end
      
      # Ensure localhost is always in the allowed IPs
      redis.sadd(key, '127.0.0.1')
      
      # Verify the migration
      new_type = redis.type(key)
      new_members = redis.smembers(key)
      puts "✓ Migrated \#{key} from \#{current_type} to \#{new_type}"
      puts "✓ Current members: \#{new_members.inspect}"
    else
      puts "✓ Key \#{key} is already a set"
      # Ensure localhost is in the set
      redis.sadd(key, '127.0.0.1')
    end
  else
    puts "Key \#{key} does not exist, creating with default value"
    redis.sadd(key, '127.0.0.1')
  end
  
  # Check and fix other related keys that might cause similar issues
  related_keys = ['banned_ips_list', 'allowed_ips_list', 'maintenance_mode_ips']
  related_keys.each do |related_key|
    next unless redis.exists(related_key) > 0
    
    current_type = redis.type(related_key)
    if current_type != 'set' && current_type != 'none'
      puts "Found related key \#{related_key} with type \#{current_type}, converting to set"
      
      # Backup and convert similar to main key
      backup_key = "\#{related_key}_backup_\#{Time.now.to_i}"
      values = []
      
      case current_type
      when 'string'
        value = redis.get(related_key)
        redis.set(backup_key, value)
        begin
          parsed = JSON.parse(value)
          values = parsed.is_a?(Array) ? parsed : [value]
        rescue JSON::ParserError
          values = [value]
        end
      when 'list'
        values = redis.lrange(related_key, 0, -1)
        values.each { |v| redis.rpush(backup_key, v) }
      end
      
      redis.del(related_key)
      values.each { |v| redis.sadd(related_key, v.strip) if v && !v.strip.empty? }
      puts "✓ Converted \#{related_key} to set"
    end
  end
  
  puts "✓ Redis to KeyDB migration completed successfully"
  
rescue => e
  puts "✗ Migration failed: \#{e.message}"
  puts "Backtrace: \#{e.backtrace.join("\\n")}"
  exit 1
end
RUBY_SCRIPT

    # Write the migration script to a temporary file and execute it
    migration_file = "/tmp/redis_keydb_migration_#{Time.now.to_i}.rb"
    File.write(migration_file, migration_script)
    
    begin
      # Execute the migration script
      run_command("/opt/opscode/embedded/bin/ruby #{migration_file}")
      log "✓ Redis to KeyDB data type migration completed successfully"
      
      # Clean up temporary file
      File.delete(migration_file) if File.exist?(migration_file)
      
      # Restart nginx to clear cached errors
      log "Restarting nginx to clear cached WRONGTYPE errors"
      stop_services(['nginx'])
      sleep 5
      start_services(['nginx'])
      
      log "✓ Redis to KeyDB migration completed - WRONGTYPE errors should be resolved"
      
    rescue => e
      log "✗ Redis to KeyDB migration failed: #{e.message}"
      # Clean up temporary file even on failure
      File.delete(migration_file) if File.exist?(migration_file)
      raise e
    end
  end
end
