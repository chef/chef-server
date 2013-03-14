## 

## 1) Preparation

### 1.1) Hosts and Access
* Determine mysql master and postgres master host names. Referred to as
  MYSQL-MASTER-HOST and POSTGRESQL-MASTER-HOST respectively. 
* Have available the database RW password from data bag secrets: chef\_db.rw\_password 
* Each person performing production validation must have a production
  org configured for testing.
* Any remote person performing valdiation should be logged into the VPN
  and SSHd into the host ``migration-validator-dev-ov-797f67ef.opscode.us``

### 1.2) Pre-run CCR and Command Terminals
* ccr into all hosts and perform a chef-client run prior to making any
  changes.  In addition to the roles listed below as impacted, this
includes:
``role:chef-pgsql``
``role:opscode-lb``
``role:mysql-master``
``role:monitoring-nagios``

* ccr on mysql master to ensure that the necessary components are in place.
* clear test data from destination postgres database
* open up a command and control terminal for all the affected servers (below)
    * verify that you are connected to all of the server that you expect to be connected to

```
knife ssh "role:opscode-account \
           OR role:opscode-accountmanagement \
           OR role:opscode-support \
           OR role:opscode-org-creator \
           OR role:monitoring-nagios" \
      csshx
```

```
knife ssh  "role:opscode-erchef \
            role:opscode-chef"
```

# Implementation 

## 2) Verify that all pre-implementation steps have been completed.

## 3) Initiate Downtime

### 3.1) Suspend daemonized CCR

**CSSHX**: `sudo /etc/init.d/chef-client stop`

### 3.2) Status Update
* status update to #Operations channel
* status update to twitter
* status update to status.opscode.com

### 3.3) Place OHC into maintenance mode
* edit role ``opscode-lb`` and set ``deny_all_except_hq`` = true
* Upload the role and CCR LBs: 

```
    knife role edit opscode-lb
    knife ssh role:opscode-lb 'sudo chef-client'
```

## 4) Run Migration 

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

## 5) Update Services

### 5.1) Update Data Bags
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

### 5.2) CCR all affected roles

**CSSHX**: `sudo chef-client`

Tail and verify the logs (from the command console):

**CSSHX**:
```
sudo tail -F /var/log/oc_erchef.log \
             /var/log/opscode-chef.log \
             /var/log/opscode-account.log \
             /var/log/opscode-acct-mgmt.log \
             /var/log/opscode-support.log \
             /var/log/opscode-org-creator.log
```

## 6) Validation 

### 6.1) Opscode Account Management
* When tailing opscode account management as described above, continue
  to monitor for a period of five minutes and ensure no exceptions are
  thrown


### 6.2) Pedant
* execute pedant against OHC. 
* Allow execution to continue, but call out completion of validation
  after setup and first block of nodes tests completes successfully. 

```
    cd rs-prod
    ./bin/ohc-pedant -e rs-prod -- --smoke --focus-nodes
```

### 6.3) Batch 
* **Important**: This validation will not complete prior to bringing OHC
  back online. It may run longer than an hour.
* Ensure no early/immediate errors 
* Execute org count script (opscode-platform-debug) and verify output is
  consistent with output when script executed against mysql. 

```
    knife search node role:monitoring-nagios 
    ssh NAGIOS-MONITORING-VM
    cd /srv/opscode-platform-debug/current/orgmapper
    ./scripts/get_org_stats.rb /etc/chef/orgmapper.conf > ~/org_stats.out
```

* After completion, open org\_stats.out and verify no errors.  Download the CSV from the link provided 
in that output, and ensure it contains org/node count data.

## 7) Restore Services

### 7.1) Take OHC out of maintenance mode 
* edit role ``opscode-lb`` and set ``deny_all_except_hq`` = false
* Upload the role and CCR LBs: 

```
    knife role edit opscode-lb
    knife ssh role:opscode-lb 'sudo chef-client'
```

### 7.2) Resume daemonized CCR 

**CSSHX**: `sudo /etc/init.d/chef-client start`

### 7.3) End Downtime
* Post status to #operations
* Status update: twitter
* Status update: status.opscode.com

## 8) Post-Completion Validation 

### 8.1) WebUI

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

### 8.2) Orgmapper
Using orgmapper, delete the user created above from OHC. Assuming a user named
"migrationtest1":
     
* SSH into a box with ``role:opscode-account``
```
    cd /srv/opscode-platform-debug/current/orgmapper
    sudo bin/orgmapper /etc/chef/orgmapper.conf
    USERS.mapper.destroy(USERS['migrationtest1'])
    exit
```

* The removal of user from community site should occur after OHC services
  are back online. Follow these instructions to do so: [Delete a User](https://wiki.corp.opscode.com/display/CORP/Orgmapper+Tips+and+Tricks#OrgmapperTipsandTricks-orgmapperDeleteauser)

### 8.3) Opscode Support
At support.opscode.us: 
* Verify  paid customers list is current at
  "https://support.opscode.us/private\_chef"
* Ensure it's possible to add a paid customer
    1. Click "Paid Customers", "Add Customer" 
    1. Enter required information 
    1. Submit 
    1. Customer should now appear on the OPC customer page

### 8.4) Opscode Accountmanagement
* Sign into www.opscode.com
* Confirm that it's possible to view org plan and billing data:
    * (https://www.opscode.com/account)
* Confirm that it's possible to view and create tickets at
    * (http://www.opscode.com/support/tickets/)
* verify ``:customer`` information is correct:  
    * (http://www.opscode.com/support/tickets/new?debug=1)
```
    Customer Ifon
    {:customer_type=>"OHC", :support_plan=>"None", :customer=>"marc_test"}
```


