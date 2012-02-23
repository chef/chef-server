name "redis"
version "2.4.7"

source :url => "http://redis.googlecode.com/files/redis-2.4.7.tar.gz",
       :md5 => "6afffb6120724183e40f1cac324ac71c"

relative_path "redis-2.4.7"

make_args = ["PREFIX=/opt/opscode/embedded",
             "CFLAGS='-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include'",
             "LD_RUN_PATH=/opt/opscode/embedded/lib"].join(" ")

build do
  command ["make", make_args].join(" ")
  command ["make install", make_args].join(" ")
end
