# Taken the connect() from https://github.com/ruby/ruby/blob/ruby_2_4/lib/net/http.rb#L892-L966
# Connecting via SSL requires a Host header. It does not
# quote ipv6 literals. Although there are no official RFC for HTTP
# explicitly allowing [] to quote ipv6 literals, it seems sensible and
# some folks are implementing it
#
# Unfortunately, the Ruby standard library does not make this easy to
# monkey patch. We actually have to monkey patch the entire connect() method,
# making this brittle. It is included here because Pedant runs against HTTPS.
# Chef client uses it's own custom connection code. The other Ruby server bits
# do not use https to connect internally, and many of them will be taken out
# when they are fully ported to Erlang. We also control the Omnibus build
# process and the specific version of Ruby to use. This is considered acceptable
# until a longer-term solution (such as submitting a patch upstream) is
# implemented.

require 'net/http'
require 'timeout'

module Net
  class HTTP < Protocol
    def connect
      if proxy? then
        conn_address = proxy_address
        conn_port    = proxy_port
      else
        conn_address = address
        conn_port    = port
      end

      D "opening connection to #{conn_address}:#{conn_port}..."
      s = Timeout.timeout(@open_timeout, Net::OpenTimeout) {
        begin
          TCPSocket.open(conn_address, conn_port, @local_host, @local_port)
        rescue => e
          raise e, "Failed to open TCP connection to " +
            "#{conn_address}:#{conn_port} (#{e.message})"
        end
      }
      s.setsockopt(Socket::IPPROTO_TCP, Socket::TCP_NODELAY, 1)
      D "opened"
      if use_ssl?
        if proxy?
          plain_sock = BufferedIO.new(s, read_timeout: @read_timeout,
                                      continue_timeout: @continue_timeout,
                                      debug_output: @debug_output)
          buf = "CONNECT #{@address}:#{@port} HTTP/#{HTTPVersion}\r\n"
          # This is the only bit that we actually monkeypatch
          buf << "Host: #{host_header}\r\n"
          if proxy_user
            credential = ["#{proxy_user}:#{proxy_pass}"].pack('m0')
            buf << "Proxy-Authorization: Basic #{credential}\r\n"
          end
          buf << "\r\n"
          plain_sock.write(buf)
          HTTPResponse.read_new(plain_sock).value
          # assuming nothing left in buffers after successful CONNECT response
        end

        ssl_parameters = Hash.new
        iv_list = instance_variables
        SSL_IVNAMES.each_with_index do |ivname, i|
          if iv_list.include?(ivname) and
            value = instance_variable_get(ivname)
            ssl_parameters[SSL_ATTRIBUTES[i]] = value if value
          end
        end
        @ssl_context = OpenSSL::SSL::SSLContext.new
        @ssl_context.set_params(ssl_parameters)
        D "starting SSL for #{conn_address}:#{conn_port}..."
        s = OpenSSL::SSL::SSLSocket.new(s, @ssl_context)
        s.sync_close = true
        # Server Name Indication (SNI) RFC 3546
        s.hostname = @address if s.respond_to? :hostname=
        if @ssl_session and
           Process.clock_gettime(Process::CLOCK_REALTIME) < @ssl_session.time.to_f + @ssl_session.timeout
          s.session = @ssl_session if @ssl_session
        end
        ssl_socket_connect(s, @open_timeout)
        if @ssl_context.verify_mode != OpenSSL::SSL::VERIFY_NONE
          s.post_connection_check(@address)
        end
        @ssl_session = s.session
        D "SSL established"
      end
      @socket = BufferedIO.new(s, read_timeout: @read_timeout,
                               continue_timeout: @continue_timeout,
                               debug_output: @debug_output)
      on_connect
    rescue => exception
      if s
        D "Conn close because of connect error #{exception}"
        s.close
      end
      raise
    end
    private :connect 

    def host_header		
      if ipv6_address?(@address)		
        "[#{@address}]:#{@port}"
      else		
        "#{@address}:#{@port}"
      end		
    end		
		
    # This does not verify that the address is a		
    # valid ipv6 address. Rather, it assumes that any		
    # address string that contains a colon is an		
    # ipv6 address. Since colons are not allowed in		
    # domain names, it should fail on a name lookup.		
    # By the time this is called, the socket should be		
    # open, so whatever address is being used would		
    # technically be a valid one.		
    def ipv6_address?(addr)		
      return true if addr =~ /:/		
      return false		
    end
  end
end
