#!/usr/bin/ruby
#
# Couchdb health check.
#
# Presently, it checks for ' 404' errors, and verifies if this is the missing view problem.
# TODO
#   Look for other kinds of errors, including erlang crash dumps.
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

class CouchdbHealthMonitor < NagiosMonitorPlugin
  attr_reader :server
  attr_reader :couchlogs
  attr_reader :errors, :view_fail, :corrupt_fail
  

  def initialize()
    super("COUCHDB HEALTH V2")
    @server = 'http://localhost:5984'   
    @couchlogs = '/etc/sv/couchdb/log/main'
    
    @opts = GetoptLong.new(
                           [ '--verbose', '-v', GetoptLong::NO_ARGUMENT ],
                           [ '--server', '-s', GetoptLong::NO_ARGUMENT ],
                           [ '--logdir', '-l', GetoptLong::NO_ARGUMENT ]
                           )
    
    @opts.each do |opt,arg|
      case opt
      when '--verbose'
        self.verbose = true 
      when '--server'
        self.server = arg
      when '--logdir'
        self.couchlogs = arg
      end
    end

    puts "@server = #{@server}" if @verbose
    puts "@couchlogs = #{@couchlogs}" if @verbose

  end

  def scan_log_for_potential_errors
    cmd = "grep -e ' 404$' -e ' 500$'"
    files = []

    if (@couchlogs.class != Array) 
      logs = [@couchlogs]
    else
      logs = @couchlogs   
    end
    logs.each do |elem|
      if (FileTest.directory?(elem)) 
        files << Dir.foreach(elem).map { |f| File.join(elem,f) } .select { |f| File.file?(f) && File.readable?(f) }
      else
        files << elem
      end
    end
    @errors = `#{cmd} #{files.join(' ')}` 
    puts "#{@errors}" if @verbose
    nil;
  end

  def filter_for_view_errors
    fails = {}
    errors.each_line do |l|
      elems = l.split(' ')
      resource = elems[7]
      if (resource =~ /(.*\/_design\/.*\/_view\/[^\?]*)/)
        view = Regexp.last_match(1)
#      puts "#{view}"
        fails[view] = true
      end
    end
    @view_fail = fails.keys
    nil;
  end

  # Anything which returns an error is a candidate 
  def filter_for_db_corruption
    fails = {}
    errors.each_line do |l|
      elems = l.split(' ')
      resource = elems[7]
      code = elems[8]
      if (resource =~ /(\/[^\/]*)/)
        db = Regexp.last_match(1)
#        puts "#{db}"
        fails[db] = true
      end
    end
    @corrupt_fail = fails.keys
    nil;
  end

  def update_errors
    scan_log_for_potential_errors
    filter_for_db_corruption
    filter_for_view_errors
  end

  def check_rest(resource)
    url = URI.parse(@server + '/' + resource)
    puts "URL : #{url}" if self.verbose
    begin 
      res = Net::HTTP.get_response(url)
    rescue => e
      puts "Ex1 #{e}" if self.verbose
      return [false, nil, nil]    
    end

    begin
      json = JSON.parse(res.body,:create_additions=>false, :symbolize_names=>true)

      if (res.code_type != Net::HTTPOK) 
        [false, res.code, json] 
      else
        [true, res.code, json]
      end
    rescue => e
      puts "Ex2 #{e}" if self.verbose
      [false, res.code, res.body]    
    end
  end

  def split_view(view)
    view =~ /\/([^\/]*)\/_design\/([^\/]*)\/_view\/([^\/]*)/  
    db = Regexp.last_match(1)
    design = Regexp.last_match(2)
    viewname = Regexp.last_match(3)
    [db,design,viewname]
  end

  def check_db(db)
    # check if db itself is ok...
    r,code,res = check_rest("#{db}/_all_docs?limit=1")
    if (!r)
      # The database has a problem!
      if (code == "500") 
        # This is a fatal database error, and we should never see this on a production system.
        add_short_status "Corruption: #{db} "
        self.exit_state = :fail
        self.do_exit
      end
      if (code == "404")
        # no such database; don't worry about that for now...
        return :ok
      end
    end
    return :ok
  end

  def check_dbs()
    @corrupt_fail.each do |db|
      self.do_exit if (check_db(db) == :fail)
    end
  end

  def check_if_valid_view(db,design,view)
    r,code,res = check_rest "#{db}/_design/#{design}"
    if (!r) 

      # error; we have trouble and throw down
      # 404: no design doc for this design; probably not a couch error (look to migrations)
      if (code =="404") 
        add_long_status "Missing view (404) #{view} for #{db}"
        self.exit_state = :warn
      elsif (code =="500") 
        # 500: Always trouble...
        add_long_status "Missing view (500) #{view} for #{db}"
        self.exit_state = :fail
        # other????
      else
        add_long_status "Missing view (unk) #{view} for #{db}"
        self.exit_state = :fail
      end
      return false
    end

    if (!res[:views].has_key?(view.to_symbol))
      # no such view... an api error, rather than a couch error.
      
      false
    else 
      # there is such a view, return true
      true
    end
  end

  def check_views()
    failed_checks = {}
    view_fail.each do |view|
      db,design,viewname = split_view(view)
      if check_if_valid_view(db,design,view) == false
        next;
      end
      # db exists...
      r,code,res = check_rest("#{db}/_design/#{design}")
      puts "#{res}"
      puts "#{res[:views][viewname.to_sym]}"
      if res[:views].has_key?(viewname.to_sym)
        begin 
          r,code,res = check_rest("#{view}?limit=0")
          # only good result has a 'total rows' field
          if (!res.has_key?('total_rows')) 
            failed_checks[view] = true
            @self.exit_state = :fail
          end
        rescue
          failed_checks[view] = true
          @self.exit_state = :fail
        end
      end
    end

    if (failed_checks.length > 0) 
      add_short_status "#{failed_checks.keys.join(', ')}"
      self.exit_state = :fail
    else
      if (failed_checks.length == 0) 
        add_short_status "no views with 404's"
      else
        add_short_status "all #{failed_views.length} views with 404's responding: " + (failed_checs.map { |x| "#{x}"}).join(" ")
      end
      self.exit_state = :ok
    end
  end
end

def run_check
  cd = CouchdbHealthMonitor.new()
  cd.update_errors
  cd.check_dbs
  cd.check_and_exit
  cd.check_views
  cd.do_exit
end

if __FILE__ == $0
  begin
    run_check
  end
else
  @@cd = CouchdbHealthMonitor.new
  @@cd.verbose = true
  @@cd.update_errors
end


