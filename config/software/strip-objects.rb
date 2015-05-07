name 'strip-objects'

default_version "0.0.1"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}"
}

# remove these to shrink size
special_rmrf = %w{share/man/man3
                  share/man/man1/perl*
                  lib/erlang/lib/wx-*
                  lib/erlang/lib/megaco-*}

build do
  special_rmrf.each do |f|
    command("rm -rf #{install_dir}/embedded/#{f}", :env => env)
  end
  command("find #{install_dir}/embedded/libexec -type f |xargs strip 2> /dev/null || true")
  command("find #{install_dir}/embedded/lib -type f|grep -v erlang |xargs strip 2> /dev/null || true")
end
