name "redis"
version "2.4.7"

source :url => "http://redis.googlecode.com/files/redis-2.4.7.tar.gz",
       :md5 => "6afffb6120724183e40f1cac324ac71c"

relative_path "redis-2.4.7"

make_args = ["PREFIX=#{install_dir}/embedded",
             "CFLAGS='-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include'",
             "LD_RUN_PATH=#{install_dir}/embedded/lib"].join(" ")

build do
  command ["make", make_args].join(" ")
  command ["make install", make_args].join(" ")
end
