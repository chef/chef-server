name "libwrap"
version "7.6"

source :url => "ftp://ftp.porcupine.org/pub/security/tcp_wrappers_7.6.tar.gz",
       :md5 => "e6fa25f71226d090f34de3f6b122fb5a"

relative_path "tcp_wrappers_7.6"

########################################################################
#
# libwrap (tcp_wrappers) build instructions pulled from
# http://www.linuxfromscratch.org/blfs/view/6.3/basicnet/tcpwrappers.html
#
########################################################################
#
# patches:
# * shared_lib_plus_plus-1: Required Patch (Fixes some build issues
#   and adds building a shared library)
# * malloc-fix: replaces the `sed` command from the build instructions
#   linked above
# * makefile-dest-fix: patches the makefile to not add "/usr/" to
#   destination dir for library install and doesn't set ownership of
#   the libraries
#

build do
  patch :source => "tcp_wrappers-7.6-shared_lib_plus_plus-1.patch"
  patch :source => "tcp_wrappers-7.6-malloc-fix.patch"
  patch :source => "tcp_wrappers-7.6-makefile-dest-fix.patch"
  command "make STYLE=-DPROCESS_OPTIONS linux"
  command "make DESTDIR=#{install_dir}/embedded install-lib"
  command "make DESTDIR=#{install_dir}/embedded install-dev"
end
