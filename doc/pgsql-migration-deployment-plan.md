## 

## Initiate Downtime
* OHC into maintenance mode (how is this done?) 
 
## Run Migration 
* from mysql master, run dump.sh (``chef_mover/mysql_to_pgsql/dump.sh``) 
* gzip output and scp to pgsql master (role: chef-pgsql) 
* from pgsql master, execute  restore.sh using the unzipped dump as
  input.  (``chef_mover/mysql_to_pgsql/restore.sh``) 
 
## Switch Environment
* Suspend daemonized CCR for affected roles
* Update Data Bag: ``env[rs-preprod][chef_db_type]`` = "postgresql"
* CCR affected roles
* Re-enable daemonized CCR

## Validation 

### Pedant
* execute pedant against OHC with no or only expected errors.

### Knife
* list, show, create, delete node 
* list, show, create, delete user 

### Opscode Support
At support.opscode.us: 
* Ensure it's possible to add/delete an OPC customer
* Ensure it's possibel to add/delete a user
* Ensure users and customers lists are correct

### Opscode Accountmanagement
* verify customer data displayed from ``http://www.opscode.com/support/tickets/new?debug=1``

### Batch 
* Execute org count script (opscode-platform-debug) and verify output is
  consistent with output when script executed against mysql. 

## Rollback (Preprod Test) 
No re-export of data is necessary here, since we are not supporting
back-porting changes from Postgresql to MySQL. 

* Suspend daemonized CCR for affected roles
* ``env[rs-preprod][chef_db_type]`` = "mysql"
* CCR affected roles
* Re-enable daemonized CCR

## End Downtime
* OHC back online
* Status update 

## Affected Roles Query
Use the below query to find nodes affected by this migration: 

``'role:opscode-account OR role:opscode-accountmanagement OR role:opscode-support OR role:opscode-org-creator OR role:opscode-erchef OR role:opscode-chef'``


## Open Questions:
* Is everything that uses ``opc_users`` and ``opc_customers`` also accounted for ?

