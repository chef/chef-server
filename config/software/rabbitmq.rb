name "rabbitmq"

dependencies ["erlang",
              # TODO: "libxslt", libxslt is required to BUILD rabbitmq, but
              # it's yet to be determined whether it needs to be there
              # to run
              "rsync"]

source :url => "http://www.rabbitmq.com/releases/rabbitmq-server/v2.7.1/rabbitmq-server-generic-unix-2.7.1.tar.gz",
       :md5 => "34a5f9fb6f22e6681092443fcc80324f"

relative_path "rabbitmq-server-2.7.1"

build do
  command "mkdir -p /opt/opscode/embedded/service/rabbitmq"
  command "/opt/opscode/embedded/bin/rsync -a ./ /opt/opscode/embedded/service/rabbitmq/"

  %w{rabbitmqclt rabbitmq-env rabbitmq-multi rabbitmq-server}.each do |cmd|
    command "ln -sf /opt/opsocde/embedded/service/rabbitmq/sbin/#{cmd} /opt/opscode/embedded/bin/#{cmd}"
  end
end
