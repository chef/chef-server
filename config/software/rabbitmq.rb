name "rabbitmq"
version "2.7.1"

dependencies ["erlang",
              # TODO: "libxslt", libxslt is required to BUILD rabbitmq, but
              # it's yet to be determined whether it needs to be there
              # to run
              "rsync"]

source :url => "http://www.rabbitmq.com/releases/rabbitmq-server/v2.7.1/rabbitmq-server-generic-unix-2.7.1.tar.gz",
       :md5 => "34a5f9fb6f22e6681092443fcc80324f"

relative_path "rabbitmq-server-2.7.1"

build do
  command "mkdir -p #{install_dir}/embedded/service/rabbitmq"
  command "#{install_dir}/embedded/bin/rsync -a ./ #{install_dir}/embedded/service/rabbitmq/"

  %w{rabbitmqctl rabbitmq-env rabbitmq-multi rabbitmq-server}.each do |cmd|
    command "ln -sf /opt/opsocde/embedded/service/rabbitmq/sbin/#{cmd} #{install_dir}/embedded/bin/#{cmd}"
  end
end
