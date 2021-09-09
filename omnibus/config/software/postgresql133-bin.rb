# limitations under the License.
#

name "postgresql133-bin"

default_version "13.3"

license "PostgreSQL"
license_file "COPYRIGHT"
skip_transitive_dependency_licensing true
dependency "zlib"
dependency "openssl"
dependency "libedit"
dependency "ncurses"
dependency "libossp-uuid"
dependency "config_guess"
source url: "https://ftp.postgresql.org/pub/source/v#{version}/postgresql-#{version}.tar.bz2"
version("13.3") { source sha256: "3cd9454fa8c7a6255b6743b767700925ead1b9ab0d7a0f9dcb1151010f8eb4a1" }
relative_path "postgresql-#{version}"
build do
  env = with_standard_compiler_flags(with_embedded_path)
  short_version = version.gsub(/^([0-9]+).([0-9]+).[0-9]+$/, '\1.\2')
  update_config_guess(target: "config")
  command "./configure" \
          " --prefix=#{install_dir}/embedded/postgresql/#{short_version}" \
          " --with-libedit-preferred" \
          " --with-openssl" \
          " --with-ossp-uuid" \
          " --with-includes=#{install_dir}/embedded/include" \
          " --with-libraries=#{install_dir}/embedded/lib", env: env

  make "world -j #{workers}", env: env

  make "-C src/interfaces/libpq install", env: env  # libpq.so
  make "-C src/backend install-bin", env: env       # postgres binary
  make "-C src/timezone/tznames  install", env: env # share/timezonesets
  make "-C src/bin install", env: env               # pg_*, psql binaries
end