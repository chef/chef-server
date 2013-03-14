# Plan Execution
Assignemnts: 
  
<table>
  <tr><th>Offset</th><th>Activity</th><th>Who</th><th>Notes</th></tr>
  <tr><td>n/a</td><td>1.2</td><td>Marc</td><td></td></tr>
  <tr><td>n/a</td><td>1.3</td><td>Seth (Erchef), Stephen (Account)</td><td></td></tr>
  <tr><td>n/a</td><td>2</td><td>Marc, All</td><td></td></tr>
  <tr><td>n/a</td><td>3.1</td><td>Seth, Stephen</td><td></td></tr>
  <tr><td>00:00</td><td>3.2</td><td>Ops</td><td></td></tr>
  <tr><td>** **</td><td></td><td></td><td>Downtime Begins<td></tr>
  <tr><td>00:02</td><td>3.3</td><td>Marc</td><td></td></tr>
  <tr><td>00:03</td><td>4.1</td><td><Marc/td><td>Step 5 proceeds concurrently after this is started. Max time expected: 5 min</td></tr>
  <tr><td>00:04</td><td>5.1</td><td>Marc</td><td>Concurrent</td></tr>
  <tr><td>00:04</td><td>5.2</td><td>Stephen</td><td>Concurrent</td></tr>
  <tr><td>00:04</td><td>5.3</td><td>Seth</td><td>Concurrent</td></tr>
  <tr><td>** **<td><td></td><td></td><td>Steps 4 and 5 must be completed before proceeding</td></tr>
  <tr><td>00:10</td> <td>6.1</td> <td>Stephen</td> <td>Concurrent w/ 6.2, 6.3</td> </tr>
  <tr><td>00:10</td> <td>6.2</td> <td>Marc</td> <td></td> </tr>
  <tr><td>00:10</td> <td>6.3</td> <td>Seth/onsite</td> <td></td> </tr>
  <tr><td>00:11</td> <td>6.4</td> <td>Marc</td> <td> Concurrent w/ 6.2, 6.3</td> </tr>
  <tr><td>** **</td><td></td><td></td><td>All of Step 6 must be completed before proceeding</td></tr>
  <tr><td>00:15</td> <td>7.1</td> <td>Marc</td> <td></td> </tr>
  <tr><td>00:16</td> <td>7.2</td> <td>Ops</td> <td></td> </tr>
  <tr><td>** **</td><td></td><td></td><td>Downtime Ends<td></tr>
  <tr><td>n/a</td> <td>7.3.1</td> <td>Stephen</td> <td></td> </tr>
  <tr><td>n/a</td> <td>7.3.2</td> <td>Seth</td> <td></td> </tr>
  <tr><td>n/a</td> <td>8.1</td> <td>Marc</td> <td></td> </tr>
  <tr><td>n/a</td> <td>8.2</td> <td>Marc</td> <td></td> </tr>
  <tr><td>n/a</td> <td>8.3</td> <td>Stephen</td> <td></td> </tr>
  <tr><td>n/a</td> <td>8.4</td> <td>Seth</td> <td></td> </tr>
  <!-- <tr><td>00:00</td> <td></td> <td></td> <td></td> </tr> -->

</table>
 
# Preparation

## 1.1) Hosts and Access
* Determine mysql master and postgres master host names. Referred to as
  MYSQL-MASTER-HOST and POSTGRESQL-MASTER-HOST respectively. 
* Have available the database RW password from data bag secrets: chef\_db.rw\_password 
* Each person performing production validation must have a production
  org configured for testing.
* Any remote person performing valdiation should be logged into the VPN
  and SSHd into the host ``migration-validator-dev-ov-797f67ef.opscode.us``
* Any remote person performing validation should have the user pem file
  and knife.rb in place on the host above for: 
     * rs-prod (preprod) 
     * production org 

## 1.2) Edit data bags
1. edit ``data_bags/environments/rs-prod.json`` and modify ``chef_db_type`` to
  ``"postgresql"``
1. edit ``data_bags/vips/rs-prod.json`` and modify ``chef_db_host``
  to the "POSTGRESQL-MASTER-HOST" value retrieved above. 
1. (Prod Only) Commit to github, do not push 
 
## 1.3) Pre-run CCR and Command Terminals
1. SSH into all hosts and perform a chef-client run prior to making any
  changes.  In addition to the roles listed below as impacted, this
  includes:
    * role:chef-pgsql
    * role:opscode-lb
    * role:mysql-master
    * role:monitoring-nagios
1. clear test data from destination postgres database
1. open up a command and control terminal for all the affected servers (below)
    * verify that you are connected to all of the servers that you expect to be connected to

Command Terminal Session: **Account**

```
knife ssh "role:opscode-account \
           OR role:opscode-accountmanagement \
           OR role:opscode-support \
           OR role:opscode-org-creator \
           OR role:monitoring-nagios" \
      csshx
```

Command Terminal Session: **Erchef**

```
knife ssh  "role:opscode-erchef \
            role:opscode-chef" \
      csshx
```

# Implementation 

## 2) Verify that all pre-implementation steps have been completed.

## 3) Initiate Downtime

### 3.1) Suspend daemonized CCR

**CSSHX** Account Sesssion: ``sudo /etc/init.d/chef-client stop``
**CSSHX** Erchef Sesssion: ``sudo /etc/init.d/chef-client stop``

### 3.2) Status Update
1. status update to twitter
1. status update to status.opscode.com
1. status update to #Operations channel that this has been completed

### 3.3) Place OHC into maintenance mode
1. edit role ``opscode-lb`` and set ``deny_all_except_hq`` = true
1. Upload the role and CCR LBs: 

```
    knife role edit opscode-lb
    knife ssh role:opscode-lb 'sudo chef-client'
```

## 4) Run Migration 

1. from mysql master, run migrate.sh as follows: 

```
    cd /srv/chef-mover/mysql_to_pgsql
    ./migrate.sh POSTGRESQL-MASTER-HOST.opscode.us
```
1. If prompted to confirm SSH key for POSTGRESQL-MASTER-HOST, do so.
1. When prompted to provide password for opscode\_chef, provide the RW password obtained above. 
1. Expected run time is between 1 and 2 minutes
1. Output will be similar to the following:

```
    marc@mysqlslave-rsprod-rv-7afff549:/srv/chef-mover/mysql_to_pgsql$ ./migrate.sh chef-pgsql-rsprod-rm-471638
    gcc -Wall -c my2pg.c
    gcc -o my2pg my2pg.o
    ready for load

    Password for opscode_chef: 

    marc@mysqlslave-rsprod-rv-7afff549:/srv/chef-mover/mysql_to_pgsql$ _
```

## 5) Update Services  (Concurrent with step 4) 

### 5.1) Data Bags
1. (Prod Only) Push previous data bag updates to rs-prod branch on git.  
1. Upload: 

```
    knife data bag from file vips rs-prod.json
    knife data bag from file environments rs-prod.json
```

### 5.2) CCR "account" session

**CSSHX**: `sudo chef-client`

Tail and verify the logs (from the command console):

**CSSHX**:
```
sudo tail -F /var/log/opscode-account.log \
             /var/log/opscode-acct-mgmt.log \
             /var/log/opscode-support.log \
             /var/log/opscode-org-creator.log
```

### 5.3) CCR "erchef" session 

**CSSHX**: `sudo chef-client`

Tail and verify the logs (from the command console):

**CSSHX**:
```
sudo tail -F /var/log/oc_erchef.log \
             /var/log/opscode-chef.log 
```

## 6) Validation 

### 6.1) Opscode Account Management
1. Restart opscode account management ``role:opscode-accountmanagement`` 
1. When tailing opscode account management, ensure no errors occur
   during startup  

### 6.2) Pedant
1. execute pedant against OHC. 

```
    cd rs-prod
    ./bin/ohc-pedant -e rs-prod -- --smoke --focus-nodes
```
1. After setup and first block of node tests completes successfully, 
   call out completion of validation. 
1. Allow tests to continue running and call out any errors.

### 6.3) WebUI - onsite 
1. Log into management console with a valid user 
1. Ensure no errors in viewing and displaying nodes 

### 6.4) Batch 
1. **Important**: This validation will not complete prior to bringing OHC
   back online. It may run longer than an hour.
1. Ensure no early/immediate errors 
1. Execute org count script (opscode-platform-debug) and verify output is
   consistent with output when script executed against mysql. 
```
    knife search node role:monitoring-nagios 
    ssh NAGIOS-MONITORING-VM
    cd /srv/opscode-platform-debug/current/orgmapper
    ./scripts/get_org_stats.rb /etc/chef/orgmapper.conf > ~/org_stats.out
```
1. After completion, open org\_stats.out and verify no errors.  Download the CSV from the link provided 
in that output, and ensure it contains org/node count data.

## 7) Restore Services

### 7.1) Take OHC out of maintenance mode 
1. edit role ``opscode-lb`` and set ``deny_all_except_hq`` = false
1. Upload the role and CCR LBs: 

```
    knife role edit opscode-lb
    knife ssh role:opscode-lb 'sudo chef-client'
```

### 7.2) End Downtime
1. Status update: twitter
1. Status update: status.opscode.com
1. Post status to #operations

### 7.3) Resume daemonized CCR 
1. **CSSHX** Account: `sudo /etc/init.d/chef-client start`
1. **CSSHX** Erchef: `sudo /etc/init.d/chef-client start`

## 8) Post-Completion Validation 

### 8.1) WebUI

In WebUI, create a new user account: 
1. www.opscode.com -> Sign Up -> Free Trial -> [enter requested info]
1. Use a unique email address and company short name
1. Verify the account by email.

In WebUI, invite the new user into the existing org: 
1. Go to manage.opscode.com and log in if necessary 
1. Choose "users" tab
1. Choose "invite" tab 
1. Provide new user name 

That user should accept the pending invite via webui.  

In WebUI dissociate the user from the org: 
1. Go to manage.opscode.com
1. Choose "users" tab 
1. Choose "dissociate" link next to the newly-joined user 

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


