## 

## Preparation
* Determine mysql master and postgres master host names. Referred to as
  MYSQL-MASTER-HOST and POSTGRESQL-MASTER-HOST respectively. 
* Have available the database RW password from data bag secrets: chef\_db.rw\_password 
* Have available a production org: 
    * should have one extra user associated that can be deleted 
* ccr on mysql master to ensure that the necessary components are in place.
* clear test data from destination postgres database


## Initiate Downtime

### Status Update
* twitter and status.opscode.com

### Place OHC into maintenance mode
* edit role ``opscode-lb`` and set ``deny_all_except_hq`` = true
* Upload the role and CCR LBs: 

```
    knife role edit opscode-lb
    knife ssh role:opscode-lb 'sudo chef-client'
```

### Suspend daemonized CCR 

```
knife ssh 'role:opscode-account \
           OR role:opscode-accountmanagement \
           OR role:opscode-support \
           OR role:opscode-org-creator \
           OR role:opscode-erchef \
           OR role:opscode-chef \
           OR role:monitoring-nagios' \
      '/etc/init.d/chef-client stop'
```

## Run Migration
* from mysql master, run migrate.sh as follows: 

```
    cd /srv/chef-mover/mysql_to_pgsql
    ./migrate.sh POSTGRESQL-MASTER-HOST.opscode.us
```

* If prompted to confirm SSH key for POSTGRESQL-MASTER-HOST, do so.
* When prompted to provide password for opscode\_chef, provide the RW password obtained above. 
* Expected run time is between 1 and 2 minutes
* Output will be similar to the following:

```
    marc@mysqlslave-rsprod-rv-7afff549:/srv/chef-mover/mysql_to_pgsql$ ./migrate.sh chef-pgsql-rsprod-rm-471638
    gcc -Wall -c my2pg.c
    gcc -o my2pg my2pg.o
    ready for load

    Password for opscode_chef: 

    marc@mysqlslave-rsprod-rv-7afff549:/srv/chef-mover/mysql_to_pgsql$ _
```

## Update Services

### Update Data Bags
The environments and vips data bags must be updated with new hosts. 

* edit ``data_bags/environments/rs-prod.json`` and modify ``chef_db_type`` to
  ``"postgresql"``
* edit ``data_bags/vips/rs-prod.json`` and modify ``chef_db_host``
  to the "POSTGRESQL-MASTER-HOST" value retrieved above. 
* Upload: 

```
    knife data bag from file vips rs-prod.json
    knife data bag from file environments rs-prod.json
```

### CCR all affected roles

```
knife ssh 'role:opscode-account \
           OR role:opscode-accountmanagement \
           OR role:opscode-support \
           OR role:opscode-org-creator \
           OR role:opscode-erchef \
           OR role:opscode-chef \
           OR role:monitoring-nagios' \
      csshx
```

From command console:
```
sudo chef-client
```

Tail and verify the logs (from the command console):
```
sudo tail -F /var/log/oc_erchef.log \
             /var/log/opscode-chef.log \
             /var/log/opscode-account.log \
             /var/log/opscode-accountmanagement.log \
             /var/log/opscode-support.log \
             /var/log/opscode-org-creator.log
```

## Validation 

### Pedant
* execute pedant against OHC. No errors should occur.

```
    cd rs-prod
    ./bin/ohc-pedant -e rs-prod -- --smoke
```

### Knife
Using an org registered in prod validate the following (sample output
shown): 

#### Nodes
List, show, create, and delete nodes within an org, using knife: 

```
   knife node list
   knife node create testnode1 -d
   knife node show testnode1
   knife node delete testnode1 
```

#### Users
List and show an org's users, using knife:
```
    knife user list
    knife user show marcparadise

```

In WebUI, create a new user account: 
* www.opscode.com -> Sign Up -> Free Trial -> [enter requested info]
* Use a unique email address and company short name
* Verify the account by email.

In WebUI, invite the new user into the existing org: 
* Go to manage.opscode.com and log in if necessary 
* Choose "users" tab
* Choose "invite" tab 
* Provide new user name 

That user should accept the pending invite via webui.  

In WebUI dissociate the user from the org: 
* Go to manage.opscode.com
* Choose "users" tab 
* Choose "dissociate" link next to the newly-joined user 

Using orgmapper, delete the user from OHC:
* Follow these directions: [Delete a User](https://wiki.corp.opscode.com/display/CORP/Orgmapper+Tips+and+Tricks#OrgmapperTipsandTricks-orgmapperDeleteauser)
* The removal of user from community site can occur after OHC services
  are back online.  

### Opscode Support
At support.opscode.us: 
* Verify that paid customers list is current at
  "https://support.opscode.us/private_chef"
* Ensure it's possible to add a paid customer
    1. Click "Paid Customers" , "Add Customer" 
    1. Enter required information 
    1. Submit 
    1. Customer should now appear on the OPC customer page


### Opscode Accountmanagement
* Sign into www.opscode.com
* verify ``:customer`` information is correct from ``http://www.opscode.com/support/tickets/new?debug=1``

```
    Customer Ifon
    {:customer_type=>"OHC", :support_plan=>"None", :customer=>"marc_test"}
```

### Batch 
* Execute org count script (opscode-platform-debug) and verify output is
  consistent with output when script executed against mysql. 
* **Important**: This validation will not complete prior to bringing OHC
  back online. It may run longer than an hour. 

```
    knife search node role:monitoring-nagios 
    ssh NAGIOS-MONITORING-VM
    cd /srv/opscode-platform-debug/current/orgmapper
    ./scripts/get_org_stats.rb /etc/chef/orgmapper.conf > ~/org_stats.out
```

After completion, open org_stats.out and verify no errors.  Download the CSV from the link provided 
in that output, and ensure it contains org/node count data.

## Restore Services

### Take OHC out of maintenance mode 
* edit role ``opscode-lb`` and set ``deny_all_except_hq`` = false
* Upload the role and CCR LBs: 

```
    knife role edit opscode-lb
    knife ssh role:opscode-lb 'sudo chef-client'
```

### Resume daemonized CCR 

```
knife ssh 'role:opscode-account \
           OR role:opscode-accountmanagement \
           OR role:opscode-support \
           OR role:opscode-org-creator \
           OR role:opscode-erchef \
           OR role:opscode-chef \
           OR role:monitoring-nagios' \
      '/etc/init.d/chef-client start'
```

### End Downtime
* Status update: twitter, status.opscode.com
