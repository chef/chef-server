echo "deb http://apt.postgresql.org/pub/repos/apt/ xenial-pgdg main" > /etc/apt/sources.list.d/pgdg.list
wget --quiet https://www.postgresql.org/media/keys/ACCC4CF8.asc
apt-key add ACCC4CF8.asc
apt-get update
apt-get install postgresql-9.6 -y
echo "host    all             all             $1/32         md5" >> /etc/postgresql/9.6/main/pg_hba.conf
echo "listen_addresses='*'" >> /etc/postgresql/9.6/main/postgresql.conf
# To turn ssl on in the external postgresql setup uncomment the following line.
echo "ssl=on" >> /etc/postgresql/9.6/main/postgresql.conf
# For the dev setup you can use the nginx certs.
# Copy them to /var/opt/opscode/postgresql/9.6/ and chown opscode-pgsql.
# Will also need to chmod 0600 these files.
#ssl_cert_file = '/var/opt/opscode/postgresql/9.6/api.chef-server.dev.crt'
#ssl_key_file  = '/var/opt/opscode/postgresql/9.6/api.chef-server.dev.key'
service postgresql restart
export PATH=/usr/lib/postgresql/9.6/bin:$PATH
sudo -u postgres psql -c "CREATE USER bofh SUPERUSER ENCRYPTED PASSWORD 'i1uvd3v0ps';"
