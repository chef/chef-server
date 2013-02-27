cd /srv/chef-mover/mysql_to_pgsql
sudo make

# Running as root will fail to connect over ssh
if [[ $EUID -eq 0 ]]; then
   echo "This script must not be run as root"
   exit 1
fi

read -s -p "Password for opscode_chef: " DB_PASSWORD

# make sure host key gets prompted for before we start piping data, if
# necessary
ssh $1 'echo connected, ready for load' 

./dump.sh $DB_PASSWORD | ssh $1 'PGPASSWORD=$DB_PASSWORD psql -h localhost -U opscode_chef -d opscode_chef -w' 
