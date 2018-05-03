pkg_name=dbdpg
pkg_origin=chef
pkg_version="3.7.4"
pkg_maintainer="The Chef Automate Maintainers <support@chef.io>"
pkg_license=('Artistic-1.0-Perl' 'GPL-2.0')
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
pkg_description="DBD::Pg is a Perl module that works with the DBI module to provide access to PostgreSQL databases."
pkg_upstream_url="http://search.cpan.org/dist/DBD-Pg/"

do_setup_environment() {
  push_buildtime_env PERL5LIB "${pkg_prefix}/lib/perl5/x86_64-linux-thread-multi"
  push_runtime_env PERL5LIB "${pkg_prefix}/lib/perl5/x86_64-linux-thread-multi"
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
