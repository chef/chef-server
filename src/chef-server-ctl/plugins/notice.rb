#
# Copyright:: Copyright (c) Chef Software, Inc.
#
# All Rights Reserved
#

add_command_under_category "notice", "general", "Display important notice and license information.", 2 do
  notice_message = <<~NOTICE
    SHAHID ---> You are good
    Please accept all the license here written
    Please consider this notice
  NOTICE
  
  puts notice_message
end
