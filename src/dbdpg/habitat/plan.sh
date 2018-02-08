pkg_name=dbdpg
pkg_origin=chef-server
pkg_version="3.5.3"
pkg_maintainer="The Chef Automate Maintainers <support@chef.io>"
pkg_license=('Artistic-1.0-Perl' 'GPL-2.0')
pkg_source=nosuchfile.tgz
pkg_shasum=7e98a9b975256a4733db1c0e974cad5ad5cb821489323e395ed97bd058e0a90e
pkg_deps=(
  core/glibc
  core/perl
  core/postgresql
  core/zlib
)
pkg_build_deps=(
  core/cpanminus
  core/local-lib
  core/gcc
  core/make
)
pkg_lib_dirs=(lib/perl5/x86_64-linux-thread-multi)
pkg_description="DBD::Pg is a Perl module that works with the DBI module to provide access to PostgreSQL databases."
pkg_upstream_url="http://search.cpan.org/dist/DBD-Pg/"

do_download() {
  return 0
}

do_unpack() {
  return 0
}

do_verify() {
  return 0
}

do_build() {
  return 0
}

do_install() {
  source <(perl -I"$(pkg_path_for core/local-lib)/lib/perl5" -Mlocal::lib="$(pkg_path_for core/local-lib)")
  source <(perl -I"$(pkg_path_for core/cpanminus)/lib/perl5" -Mlocal::lib="$(pkg_path_for core/cpanminus)")
  source <(perl -Mlocal::lib="$pkg_prefix")
  cpanm "DBD::Pg@$pkg_version" --local-lib "$pkg_prefix"
}
