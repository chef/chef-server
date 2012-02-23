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

build do
  command ["./configure",
           "--prefix=/opt/opscode/embedded",
           "--disable-doc-dot",
           "--disable-doc-search",
           "--disable-doc-tagfile",
           "--disable-doc-chm",
           "--disable-doc-docset",
           "--disable-qt",
           "--disable-examples"].join(" ")
  command "make", :env => { "LD_RUN_PATH" => "/opt/opscode/embedded/lib" }
  command "make install"
end
