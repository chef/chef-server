#!/bin/sh

#### IMPORTANT:
#
# Updated PostgreSQL version numbering:
#  * Uses 'major1.major2.minor' for <=9.6.x
#  * Uses 'major.minor' for >= 10.x
#
# i.e., major PostgreSQL releases include:
#   '9.5, '9.6', '10', '11', '12', '13', '14', etc.

#### ENVIRONMENT VARIABLES
# The following environment variables are available for overrides
# through Vagrant, or changed below, otherwise defaults are set:
#
#CS_IP=
#     Chef Infra Server IP address
#     default: '192.168.33.100'
#
#PG_MAJOR=
#     PostgreSQL major version
#     default: '13'
#
#PG_MINOR=
#     PostgreSQL minor version (e.g., for 13.2 -> PG_MINOR=2)
#     values: 'latest' [default], integer >= 0
#
#PG_HBA_AUTH=
#     Password auth method for connections in pg_hba.conf
#     Ref: https://www.postgresql.org/docs/14/auth-password.html
#     values:
#         'md5' [default]
#         'scram-sha-256' (available >= PG10)
#
#DB_SUPERUSER=
#     Database superuser username
#     default: 'bofh'
#
#DB_SUPERPASS=
#     Database superuser password
#     default: 'i1uvd3v0ps'
#
#PW_ENCRYPTION=
#     Password hashing algorithm (may differ from PG_HBA_AUTH)
#     default: <empty> (use PG compiled defaults)
#     values:
#         'md5'           [default: <= PG13]
#         'scram-sha-256' [default: >= PG14] (available >= PG10)
#
#     Note: 'scram-sha-256' may require library updates for support,
#       such as ruby-pg, epgsql, etc.

#
#### Set default values (if not overridden by environment variables)
#

test -n "${CS_IP}"        || CS_IP=${1:-'192.168.33.100'} # Allow old $1 parameter
test -n "${PG_MAJOR}"     || PG_MAJOR=13
test -n "${PG_MINOR}"     || PG_MINOR='latest'
test -n "${PG_HBA_AUTH}"  || PG_HBA_AUTH='md5'
test -n "${DB_SUPERUSER}" || DB_SUPERUSER='bofh'
test -n "${DB_SUPERPASS}" || DB_SUPERPASS='i1uvd3v0ps'

# Grab DISTRIB_CODENAME value from /etc/lsb-release
test -f /etc/lsb-release && . /etc/lsb-release
# Otherwise, set a default value
test -n "${DISTRIB_CODENAME}" || DISTRIB_CODENAME='bionic'

#
#### Set up repositories
#

# Add PGDG repository (for latest PG minor releases only)
echo "deb http://apt.postgresql.org/pub/repos/apt/ ${DISTRIB_CODENAME}-pgdg main" > /etc/apt/sources.list.d/pgdg.list

# Add PGDG archive repository (for older PG minor releases)
echo "deb http://apt-archive.postgresql.org/pub/repos/apt/ ${DISTRIB_CODENAME}-pgdg-archive main" > /etc/apt/sources.list.d/pgdg-archive.list

# Add apt_preferences file for pgdg-archive repo, setting a lower apt policy
# priority to avoid installing older packages when not requested
cat << EOF > /etc/apt/preferences.d/pgdg-archive.pref
Package: *
Pin: release o=apt-archive.postgresql.org
Pin-Priority: 100
EOF

# Install PGDG repository apt key
wget -qO - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -

#
#### Install PostgreSQL packages
#

# Update local apt package cache
apt-get update

# Install the appropriate PostgreSQL package version
if [ -z "${PG_MINOR}" ] || [ "${PG_MINOR}" = "latest" ]
then
  apt-get install postgresql-${PG_MAJOR} -y
else
  apt-get install postgresql-${PG_MAJOR}=${PG_MAJOR}.${PG_MINOR}-* -y
fi

#
#### Configure PostgreSQL
#

# Permit access from Chef Infra Server IP
#     TYPE  DATABASE  USER  ADDRESS      METHOD
echo "host  all       all   ${CS_IP}/32  ${PG_HBA_AUTH}" >> /etc/postgresql/${PG_MAJOR}/main/pg_hba.conf

# Listen to all interfaces/addresses
echo "listen_addresses='*'" >> /etc/postgresql/${PG_MAJOR}/main/postgresql.conf

# Set user password hashing algorithm (valid for PG10+)
if [ -n "${PW_ENCRYPTION}" ] && [ "$(echo $PG_MAJOR | cut -f1 -d.)" -ge 10 ] # minimum version hack
then
  echo "password_encryption='${PW_ENCRYPTION}'" >> /etc/postgresql/${PG_MAJOR}/main/postgresql.conf
fi

# Use postgresql-common script to initiate systemd start/restart of cluster
pg_ctlcluster ${PG_MAJOR} main restart

#
#### Set up database superuser
#

export PATH=/usr/lib/postgresql/${PG_MAJOR}/bin:$PATH
sudo -u postgres psql -c "CREATE USER ${DB_SUPERUSER} SUPERUSER ENCRYPTED PASSWORD '${DB_SUPERPASS}';"
