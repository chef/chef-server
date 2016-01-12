
* TODO: Automate db creation and bookshelf init.
Until then:

`sudo su opscode-pgsql -c "/opt/opscode/embedded/bin/psql bookshelf"`

In sql
```
DROP DATABASE bookshelf;
CREATE DATABASE bookshelf;
GRANT ALL PRIVILEGES ON DATABASE bookshelf TO opscode_chef;
SET ROLE opscode_chef;
```


```
export PGPASSWORD=8a18ecf50ecd37087c2ad1841439b329ea591559653de4b3bce1156f3c00
sqitch --db-name bookshelf -u opscode_chef --db-host localhost deploy
```


* TODO: Make real test suite
curl -i -X PUT  localhost:4321/foo/README.md3  -d@README.md


* TODO: Write up debugging notes
redbug:start("bksw_sec:is_authorized->stack,return",[{time,180000}, {msgs, 10}]).




* TODO Usage for s3cmd 

s3cmd -d -v --config /vagrant/s3cfg la s3://
s3cmd -d -v --config /vagrant/s3cfg ls
s3cmd -d -v --config /vagrant/s3cfg ls s3://bar
s3cmd -d -v --config /vagrant/s3cfg ls s3://bucket/foo
s3cmd -d -v --config /vagrant/s3cfg ls s3://foo
s3cmd -d -v --config /vagrant/s3cfg ls s3://foo.api
s3cmd -d -v --config /vagrant/s3cfg ls s3://foo.api/
s3cmd -d -v --config /vagrant/s3cfg ls s3://foo/bucket
s3cmd -d -v --config /vagrant/s3cfg mb boing
s3cmd -d -v --config /vagrant/s3cfg mb s3://api/boing
s3cmd -d -v --config /vagrant/s3cfg mb s3::/boing
s3cmd -d -v --config /vagrant/s3cfg mb s3:/boing
s3cmd -d -v --config /vagrant/s3cfg mb s3://boing

/Users/mark/Notes/2015-11-24.org
2:s3cmd -d -v --config /vagrant/s3cfg la s3://

/Users/mark/Notes/2015-11-24.org~
2:s3cmd -d -v --config /vagrant/s3cfg la s3://

* TODO Common tests:
sudo bash
cd /host/src/bookshelf
DEBUG=0 ./rebar3 ct

(May need to 'dvm start bookshelf' in another window)


* TODO Make port forwarding possible via DVM
 ssh vagrant@127.0.0.1 -p 2222 -o Compression=yes -o DSAAuthentication=yes -o LogLevel=FATAL -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o IdentitiesOnly=yes -i /home/mark/oc/chef-server/dev/.vagrant/machines/chef-server/virtualbox/private_key -o ForwardAgent=yes -L4321:localhost:4321

* Failing tests:
Intermittent:

%%% bkswt_api_SUITE ==> bucket_many: FAILED
%%% bkswt_api_SUITE ==> 
Failure/Error: ?assertEqual([[97,102,101,105,122,112,98,105,107,109,111,97,107,108,102,111,101,115,102,100,112,109,114,120,112,105,110,121,115,108],[98,117,107,107,105,116],[98,117,107,107,105,116,45,112,109,114,120,112,105,110,121,115,108]], bucket_list ( S3Conf ))
  expected: ["afeizpbikmoaklfoesfdpmrxpinysl","bukkit","bukkit-pmrxpinysl"]
       got: ["bukkit","bukkit-pmrxpinysl"]      line: 191

