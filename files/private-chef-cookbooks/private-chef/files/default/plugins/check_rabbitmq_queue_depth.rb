#!/usr/bin/ruby
#
# Rabbitmq job queue depth check.
# 
#

require 'rubygems'
require 'json'
require 'net/http'
require 'uri'
require 'pathname'
require 'getoptlong'

class NagiosMonitorPlugin 
  attr_accessor :verbose
  attr_reader :cmdpath, :exitstate
  attr_reader :monitor_name
  attr_reader :short_status
  attr_reader :long_status

  def initialize(monitor_name)
    @exitstate = :ok
    @verbose = false
    @cmdpath = Pathname.new(__FILE__).dirname.realpath
    @monitor_name = monitor_name
    clr_message
  end

  def exit_state=(state)
    if (@exitstate == :ok) ||
        (@exitstate == :warn && state == :fail) ||
        (@exitstate == :warn && state == :unknown) 
      @exitstate = state
    end
  end
  
  def reset_exit_state()
    @exitstate = :ok
  end
  
  def gen_message()
    puts "#{@monitor_name} #{@exitstate.to_s.upcase} - #{@short_status}"
    puts "#{@long_status}" if (!@long_status.nil? && @long_status.length > 0)
  end

  def clr_message()
    @short_status = ""
    @long_status = ""
  end

  def check_and_exit()
    do_exit() if @exitstate != :ok
  end

  def do_exit()
    gen_message
    case @exitstate
    when :ok
      exit(0)
    when :warn
      exit(1)
    when :fail
      exit(2)
    when :unknown
      exit(3)
    end
  end

  def add_short_status(append)
    @short_status += append
  end
  def add_long_status(append)
    @long_status += append
  end

end

class RabbitMQQueueMonitor < NagiosMonitorPlugin
  attr_reader :warn_level
  attr_reader :crit_level
  attr_reader :queue_name

  def initialize()
    super("RABBITMQ QUEUE DEPTH")
    @opts = GetoptLong.new(
                           [ '--verbose', '-v', GetoptLong::NO_ARGUMENT ],
                           [ '--warn', '-w', GetoptLong::OPTIONAL_ARGUMENT ],
                           [ '--crit', '-c', GetoptLong::OPTIONAL_ARGUMENT ],
                           [ '--queue', '-q', GetoptLong::OPTIONAL_ARGUMENT ]
                           )
    @warn_level = 10
    @crit_level = 20

    @opts.each do |opt,arg|
      case opt
      when '--verbose'
        self.verbose = true 
      when '--warn'
        @warn_level = arg.to_i
      when '--crit'
        @crit_level = arg.to_i
      when '--queue'
        @queue_name = arg
      end
    end

  end

  def check_queue_depth
    cmd = "/usr/local/sbin/rabbitmqctl list_queues" + 
      if self.queue_name.nil? then "" else " -p #{queue_name}" end
    queue_list = `#{cmd}`
    if (not $?.success?) 
      add_short_status "Command failed"
      self.exit_state = :fail
      return
    end
    queue_list.each_line do |line|
      line.chomp!
      next if line =~ /\.\.\./
      queue, depths = line.split(' ')
      depth = depths.to_i
      if depth >= crit_level
        add_short_status "#{queue_name} #{queue} has #{depth} entries"
        self.exit_state = :fail
        next
      end
      if depth >= warn_level
        add_short_status "#{queue_name} #{queue} has #{depth} entries"
        self.exit_state = :warn
        next
      end
    end
  end

  
end

def run_check
  cd = RabbitMQQueueMonitor.new()
  cd.check_queue_depth 
  cd.do_exit
end

if __FILE__ == $0
  begin
    run_check
  end
else
  @@cd = RabbitMQQueueMonitor.new
  @@cd.verbose = true
  @@cd.update_errors
end


