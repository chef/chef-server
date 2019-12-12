module ChefServer
  class Warnings
    @@warnings = []

    def self.warn(msg)
      @@warnings << msg
    end

    def self.print_warnings
      return if @@warnings.empty?

      puts "\n" + '-' * 70 + "\n"
      puts <<~EOF

        The following warnings were encountered during the reconfiguration of
        your Chef server:

      EOF
      @@warnings.each do |msg|
        puts "#{msg}\n"
      end

      puts "\n" + '-' * 70 + "\n"
    end
  end
end
