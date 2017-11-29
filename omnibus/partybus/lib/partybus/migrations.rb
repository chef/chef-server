require 'json'
require 'partybus/dsl_runner'

module Partybus

  module MigrationComparable

    include Comparable

    attr_reader :major
    attr_reader :minor

    def <=>(other)
      maj_comp = @major <=> other.major
      if maj_comp == 0
        @minor <=> other.minor
      else
        maj_comp
      end
    end

    def to_s
      "#{@major}.#{@minor}"
    end

  end

  class MigrationFile

    include MigrationComparable

    attr_reader :path

    def initialize(path)
      @path = path
      @major, @minor = parse_path(path)
    end

    def run_migration(migration_state)
      runner = DSLRunner.new(self)
      runner.run(migration_state)
    end

    def run_check
      runner = DSLRunner.new(self)
      runner.check
    end

    private

    def parse_path(p)
      minor = File.basename(p, ".rb")[/^(\d+)/, 1].to_i
      major = File.basename(File.dirname(p)).to_i
      [major, minor]
    end
  end

  class MigrationState

    include MigrationComparable

    def initialize(path)
      @major, @minor = begin
                         json_data = JSON.parse(IO.read(path))
                         [json_data["major"], json_data["minor"]]
                       rescue JSON::ParserError => e
                         [0, 0]
                       end
    end

  end
end
