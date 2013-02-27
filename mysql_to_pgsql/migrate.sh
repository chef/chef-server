cd /srv/chef-mover/mysql_to_pgsql
sudo make

# Running as root will fail to connect over ssh
if [[ $EUID -eq 0 ]]; then
   echo "This script must not be run as root"
   exit 1
fi

# Quick and dirty way to make sure host key is added
ssh $1 'echo ready for load' 
echo ""

read -s -p "Password for opscode_chef: " DB_PASSWORD


./dump.sh $DB_PASSWORD | ssh $1 'PGPASSWORD=$DB_PASSWORD psql -h localhost -U opscode_chef -d opscode_chef -w' 
