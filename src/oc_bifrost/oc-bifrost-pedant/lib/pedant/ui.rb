# Copyright: Copyright 2012-2018 Chef Software, Inc.
# License: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

module Pedant
  class UI

    # Everybody needs a logo!
    def logo
  <<-EOM
 _______  _______  _______  _______  _______  ______   _______
|       ||       ||       ||       ||       ||      | |       |
|   _   ||    _  ||  _____||       ||   _   ||  _    ||    ___|
|  | |  ||   |_| || |_____ |       ||  | |  || | |   ||   |___
|  |_|  ||    ___||_____  ||      _||  |_|  || |_|   ||    ___|
|       ||   |     _____| ||     |_ |       ||       ||   |___
|_______||___|    |_______||_______||_______||______| |_______|

     _______  _______  ______   _______  __    _  _______
    |       ||       ||      | |   _   ||  |  | ||       |
    |    _  ||    ___||  _    ||  |_|  ||   |_| ||_     _|
    |   |_| ||   |___ | | |   ||       ||       |  |   |
    |    ___||    ___|| |_|   ||       ||  _    |  |   |
    |   |    |   |___ |       ||   _   || | |   |  |   |
    |___|    |_______||______| |__| |__||_|  |__|  |___|
EOM
    end

    def info_banner
  <<-EOM
#{logo}
                    "Accuracy Over Tact"

                  === Testing Environment ===
                 Config File: #{File.absolute_path(Pedant.config.config_file)}
       #{Pedant.config.log_file ? "HTTP Traffic Log File: " + File.absolute_path(Pedant.config.log_file) + "\n" : ""}
EOM
    end

  end
end
