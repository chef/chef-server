class HAProxyStatus

  attr_accessor :socket
  def initialize(sock)
    @socket = sock
  end

  def server_stats
    stats(" -1 4 -1")
  end

  def stats(args=nil)
    socket.puts("show stat#{args}")
    parse_stats_table(read_until_end(socket))
  end

  private
  def read_until_end(socket)
    ret = []
    while line = socket.gets
      ret << line
    end
    ret
  end

  def parse_stats_table(table)
    return [] if table.empty?
    header, *data = table
    header = transform_header(header)
    data.map do |line|
      parse_status_line(line, header)
    end.compact
  end

  def transform_header(line)
    columns = line.split(",").map(&:strip)
    columns[0] = columns[0].gsub("# ", "")
    columns
  end

  # Incomplete parser for the output of show stats;
  #
  # Currently only returns the pxname, svname, status
  #
  def parse_status_line(line, header)
    split_line = line.split(",").map(&:strip)
    if split_line.first == "" && split_line.length == 1 # Empty line
      nil
    else
      { pxname: split_line[header.index("pxname")],
        svname: split_line[header.index("svname")],
        status: split_line[header.index("status")] }
    end
  end
end
