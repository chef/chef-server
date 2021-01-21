+++
title = "Deploy Mutual TLS Certificates"

date = 2021-01-05T15:18:55-08:00
draft = true

[menu]
  [menu.server]
    title = "Mutual TLS"
    identifier = "server/security/mutual_tls_deploy"
    parent = "server/security"

+++

### Add the Root CA, Intermediate CA and Server Certs to your local System

Now you need to add the certificates from your new Root and Intermediate CA to your chef workstation.

##### MacOS:

Import all three certificates into the Local keychain. Import the Root CA first. Note that you will have to manually set that certificate as Trusted. The Intermediate CA certificate should then indicate that it is trusted by default once you import it. And finally, import the server certificate

##### Windows:

Open Certmgr against the Local Machine and import the Root and Intermediate CA certificates into the [Trusted Root Certification Authorities\Certificates] node

Then add the server certificate into [Local Machine\Personal\Certificates]

### Test Your Connection Again - It Works!

You should now see the generic Chef Server landing page that has the title "Are You Looking for Chef Infra Server?"

### Adding Support for Mutual TLS Now

Now that we are using our own managed certificates to secure the server with, we want to add support for individual users/nodes to securely connect with. The first step is to create a user-specific certificate-key pair and for testing, we'll create a user-specific p12 from that key-pair.

[Reference]: https://www.golinuxcloud.com/openssl-create-client-server-certificate/

```bash
$ cd cd /root/ca/intermediate
$ openssl genrsa -out private/billg.key.pem 4096
$ openssl req -new -key private/billg.key.pem -out billg.csr
You are about to be asked to enter information that will be incorporated
into your certificate request.
What you are about to enter is what is called a Distinguished Name or a DN.
There are quite a few fields but you can leave some blank
For some fields there will be a default value,
If you enter '.', the field will be left blank.
-----
Country Name (2 letter code) [AU]:US
State or Province Name (full name) [Some-State]:WA
Locality Name (eg, city) []:Seattle
Organization Name (eg, company) [Internet Widgits Pty Ltd]:Friedlander Hosiery
Organizational Unit Name (eg, section) []:
Common Name (e.g. server FQDN or YOUR name) []:Bill Gates
Email Address []:billg@friedlanderhosiery.com

Please enter the following 'extra' attributes
to be sent with your certificate request
A challenge password []:
An optional company name []:Friedlander Hosiery

$ openssl ca -config openssl.cnf \
            -extensions usr_cert \
            -days 375 -notext -md sha256 \
            -in billg.csr \
            -out certs/billg.cert.pem
```

Now verify the certificate, key and certificate request with the following commands

```bash
$ cd /root/ca/intermediate
$ openssl rsa -noout -text -in private/billg.key.pem
$ openssl req -noout -text -in billg.csr
$ openssl x509 -noout -text -in certs/billg.cert.pem
```

### Optional But Recommended - Convert the user keypair to P12

One way to test connectivity between your chef workstation and the Chef Server AFTER adding support for mutual TLS is to create a .P12 file from your user-specific certificate-key pair. You'll load that into the appropriate keystore on your client and then you can attempt to connect to your Chef Server in a browser. When configured properly, the browser will prompt you to select the client certificate (i.e. your P12) and will attempt to connect from there.

```bash
$ openssl pkcs12 -export -out certs/billg.p12 -inkey private/billg.key.pem -in certs/billg.cert.pem

Enter Export Password:
Verifying - Enter Export Password:
```

##### MacOS:

First run this on billg.p12

```bash
$ chmod a+r billg.p12
```

Then import that file into your Local Keychain

##### Windows:

Open Certmgr against the Current User and put the p12 file into [Current User\Personal\Certificates]

### Turning on TLS Support on the Chef Server

The final piece needed to enable mutual TLS support is to go back to the chef-server.rb file and add the following line to it. Please also add a value for the verify_depth to describe how many Certificate Authorities exist on top of your Server Certificate - In our case here, we have a Root CA and an Intermediate CA so our depth value is 2.

```ruby
nginx['ssl_client_ca'] = "/your/path/ca.cert.pem"
nginx['ssl_verify_depth'] = 2
```

Now we need to update Chef Server to accept that last change. Run this again:

```bash
$ chef-server-ctl reconfigure
```

When the services have all restarted correctly you can point your browser at https://yourserver and browser should ask you which client certificate to use and once you supply the p12 you just imported you should see the default landing page again.

### Configuring local settings for chef-client and knife

The final configuration piece you need now that TLS is setup is to configure your local system to use the correct certificates to communicate to the Chef Server with. These settings can be used in either your chef-client.rb or your /users/you/.chef/credentials file.

Note that you created your user with the Intermediate CA so you'll only need to use and specify the Root CA in the configuration.

```ruby
ssl_ca_path = "C:\\opscode"
ssl_ca_file = "C:\\opscode\\ca.cert.pem"
ssl_client_cert = "C:\\opscode\\billg.cert.pem"
ssl_client_key = "C:\\opscode\\billg.key.pem"
ssl_verify_mode = 'verify_peer'
```
