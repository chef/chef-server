name "nagios"
version "3.3.1"

dependencies ["gd", "php"]

source :url => "http://iweb.dl.sourceforge.net/project/nagios/nagios-3.x/nagios-3.3.1/nagios-3.3.1.tar.gz",
       :md5 => "c935354ce0d78a63bfabc3055fa77ad5"

relative_path "nagios"

env = {
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  # configure it
  command(["./configure",
           "--prefix=#{install_dir}/embedded/nagios",
           "--with-nagios-user=opscode-nagios",
           "--with-nagios-group=opscode-nagios",
           "--with-command-group=opscode-nagios-cmd",
           "--with-command-user=opscode-nagios-cmd",
           "--with-gd-lib=#{install_dir}/embedded/lib",
           "--with-gd-inc=#{install_dir}/embedded/include",
           "--with-temp-dir=/var#{install_dir}/nagios/tmp",
           "--with-lockfile=/var#{install_dir}/nagios/lock",
           "--with-checkresult-dir=/var#{install_dir}/nagios/checkresult",
           "--with-mail=/usr/bin/mail"].join(" "),
          :env => env)

  # so dome hacky shit
  command "sed -i 's:for file in includes/rss/\\*;:for file in includes/rss/\\*.\\*;:g' ./html/Makefile"
  command "sed -i 's:for file in includes/rss/extlib/\\*;:for file in includes/rss/extlib/\\*.\\*;:g' ./html/Makefile"
  command "bash -c \"find . -name 'Makefile' | xargs sed -i 's:-o opscode-nagios-cmd -g opscode-nagios-cmd:-o root -g root:g'\""
  command "bash -c \"find . -name 'Makefile' | xargs sed -i 's:-o opscode-nagios -g opscode-nagios:-o root -g root:g'\""

  # build it
  command "make all", :env => { "LD_RUN_PATH" => "#{install_dir}/embedded/lib" }
  command "sudo make install"
  command "sudo make install-config"
  command "sudo make install-exfoliation"

  # clean up the install
  command "sudo rm -rf #{install_dir}/embedded/nagios/etc/*"
end
