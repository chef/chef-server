name "gecode"
version "3.7.1"

source :url => "http://www.gecode.org/download/gecode-3.7.1.tar.gz",
       :md5 => "b4191d8cfafa18bd9b78594544be2a04"

relative_path "gecode-3.7.1"

# TODO:
# * set CC and CXX based on presence of gcc version
# * ask Adam why
#
# == ominbus-pc/config/software/gecode.clj:19
#
# (let
#  [env (if (= 0 (get (clojure.java.shell/sh "test" "-f" "/usr/bin/gcc44") :exit))
#         { "CC" "gcc44" "CXX" "g++44" }
#         { })]
#

test = Mixlib::ShellOut.new("test -f /usr/bin/gcc44")
test.run_command

configure_env = if test.exitstatus == 0
                  {"CC" => "gcc44", "CXX" => "g++44"}
                else
                  {}
                end

build do
  command(["./configure",
           "--prefix=#{install_dir}/embedded",
           "--disable-doc-dot",
           "--disable-doc-search",
           "--disable-doc-tagfile",
           "--disable-doc-chm",
           "--disable-doc-docset",
           "--disable-qt",
           "--disable-examples"].join(" "),
          :env => configure_env)
  command "make -j #{max_build_jobs}", :env => { "LD_RUN_PATH" => "#{install_dir}/embedded/lib" }
  command "make install"
end
