######################################################################
# the logger
######################################################################

module Opscode::Test

  def self.logger
    @logger ||= Opscode::Test::Logger.new
  end

  module Loggable
    def log(*args)
      Opscode::Test.logger.log(*args)
    end
  end

  # basic dumb logger that does nothing but write strings to the
  # terminal, doens't even have levels
  #
  class Logger

    # log a message
    #
    # === Arguments
    # * :message: - duh
    # * :type:    - the type of message to log
    #
    # === Types
    # * :task:   - the top-level task
    # * :detail: - a detail about the task, indented one tab
    #
    def log(message, type=:task)
      if type == :detail
        message = "\t#{message}"
      end
      puts message
    end
  end
end
