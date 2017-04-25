echo "deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main" > /etc/apt/sources.list.d/pgdg.list
wget --quiet https://www.postgresql.org/media/keys/ACCC4CF8.asc
apt-key add ACCC4CF8.asc
apt-get update
apt-get install postgresql-9.2 -y
echo "host    all             all             #{IPS[:cs]}/32         md5" >> /etc/postgresql/9.2/main/pg_hba.conf
echo "listen_addresses='*'" >> /etc/postgresql/9.2/main/postgresql.conf
service postgresql restart
export PATH=/usr/lib/postgresql/9.2/bin:$PATH
sudo -u postgres psql -c "CREATE USER bofh SUPERUSER ENCRYPTED PASSWORD 'i1uvd3v0ps';"
