# Taken from the Ruby 1.9.3 standard library
# Connecting via SSL requires a Host header. It does not
# quote ipv6 literals. Although there are no official RFC for HTTP
# explicitly allowing [] to quote ipv6 literals, it seems sensible and
# some folks are implementing it.
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
      D "opening connection to #{conn_address()}..."
      s = Timeout::timeout(@open_timeout) { TCPSocket.open(conn_address(), conn_port()) }
      D "opened"
      if use_ssl?
        ssl_parameters = Hash.new
        iv_list = instance_variables
        SSL_ATTRIBUTES.each do |name|
          ivname = "@#{name}".intern
          if iv_list.include?(ivname) and
            value = instance_variable_get(ivname)
            ssl_parameters[name] = value
          end
        end
        @ssl_context = OpenSSL::SSL::SSLContext.new
        @ssl_context.set_params(ssl_parameters)

        @ssl_context.ssl_version = Pedant::Config.ssl_version || :TLSv1
        s = OpenSSL::SSL::SSLSocket.new(s, @ssl_context)
        s.sync_close = true
      end
      @socket = BufferedIO.new(s)
      @socket.read_timeout = @read_timeout
      @socket.continue_timeout = @continue_timeout
      @socket.debug_output = @debug_output
      if use_ssl?
        begin
          if proxy?
            @socket.writeline sprintf('CONNECT %s:%s HTTP/%s',
                                      @address, @port, HTTPVersion)
            @socket.writeline host_header
            if proxy_user
              credential = ["#{proxy_user}:#{proxy_pass}"].pack('m')
              credential.delete!("\r\n")
              @socket.writeline "Proxy-Authorization: Basic #{credential}"
            end
            @socket.writeline ''
            HTTPResponse.read_new(@socket).value
          end
          # Server Name Indication (SNI) RFC 3546
          s.hostname = @address if s.respond_to? :hostname=
            timeout(@open_timeout) { s.connect }
          if @ssl_context.verify_mode != OpenSSL::SSL::VERIFY_NONE
            s.post_connection_check(@address)
          end
        rescue => exception
          D "Conn close because of connect error #{exception}"
          @socket.close if @socket and not @socket.closed?
          raise exception
        end
      end
      on_connect
    end

    def host_header
      if ipv6_address?(@address)
        "Host: [#{@address}]:#{@port}"
      else
        "Host: #{@address}:#{@port}"
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

  end # HTTP
end # Net

