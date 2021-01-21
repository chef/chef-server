+++
title = "Setup Mutual TLS"

date = 2021-01-05T15:18:55-08:00
draft = true

[menu]
  [menu.server]
    title = "Setup Mutual TLS"
    identifier = "server/security/mutual_tls_config"
    parent = "server/security"

+++

## Create the Root CA

[Reference]: https://jamielinux.com/docs/openssl-certificate-authority/create-the-root-pair.html

From your chef workstation connect to your Chef Server and execute the following commands to create the necessary directory structure for your certificate server

```bash
$ mkdir /root/ca
$ cd /root/ca
$ mkdir certs crl newcerts private
$ chmod 700 private
$ touch index.txt
$ echo 1000 > serial
```

Now you need to create an openssl.cnf file for your Root CA. See the Root CA example at the end of this document for how we set that up. You'll need to configure the various fields that are marked for you.

Once that file is created and placed in the /root/ca folder, we need to create a key with which we will create the root certificate

```bash
$ cd /root/ca
$ openssl genrsa -aes256 -out private/ca.key.pem 4096

Enter pass phrase for ca.key.pem: secretpassword
Verifying - Enter pass phrase for ca.key.pem: secretpassword

$ chmod 400 private/ca.key.pem
```

Now that we have a key, we need to create the Root CA certificate

```bash
$ cd /root/ca
$ openssl req -config openssl.cnf \
      -key private/ca.key.pem \
      -new -x509 -days 7300 -sha256 -extensions v3_ca \
      -out certs/ca.cert.pem

Enter pass phrase for ca.key.pem: secretpassword
You are about to be asked to enter information that will be incorporated
into your certificate request.

-----
Country Name (2 letter code) [XX]:US
State or Province Name []:WA
Locality Name []:Seattle
Organization Name []:Friedlanders Hosiery
Organizational Unit Name []:IT
Common Name []:Friedlanders Hosiery Root CA
Email Address []:webmaster@friedlandershosiery.com

$ chmod 444 certs/ca.cert.pem
```

Notice that for the Common Name we used a full proper name and NOT a URL. This is important to do to keep various certificates distinct and distinguishable from each other.

Now you can verify the root certificate

```bash
$ openssl x509 -noout -text -in /certs/ca.cert.pem
```

### Creating the Intermediate CA

Still connected to your Chef Server from your chef workstation you need to create a new directory structure for your Intermediate CA

```bash
$ mkdir /root/ca/intermediate
$ cd /root/ca/intermediate
$ mkdir certs crl csr newcerts private
$ chmod 700 private
$ touch index.txt
$ echo 1000 > serial
```

Add a crlnumber file to your Intermediate CA to keep track of certificate revocation lists

```bash
$ echo 1000 > /root/ca/intermediate/crlnumber
```

Now you need to create an openssl.cnf file for your Intermediate CA. See the Intermediate CA example at the end of this document for how we set that up. You'll need to configure the various fields that are marked for you. You'll also need to configure SAN names for your server as illustrated in the [ alt_names ] section. Those alternative names will be used during the creation of the certificate for your chef server.

Once that file is created and placed in the /root/ca/intermediate folder, we need to create a key with which we will create the intermediate CA certificate

```bash
$ cd /root/ca
$ openssl genrsa -aes256 \
      -out intermediate/private/intermediate.key.pem 4096

Enter pass phrase for intermediate.key.pem: secretpassword
Verifying - Enter pass phrase for intermediate.key.pem: secretpassword

$ chmod 400 intermediate/private/intermediate.key.pem
```

Now we can create the certificate signing request. Notice once again that we have chosen a distinct Common Name for this CA.

```bash
$ cd /root/ca
$ openssl req -config intermediate/openssl.cnf -new -sha256 \
      -key intermediate/private/intermediate.key.pem \
      -out intermediate/csr/intermediate.csr.pem

Enter pass phrase for intermediate.key.pem: secretpassword
You are about to be asked to enter information that will be incorporated
into your certificate request.
-----
Country Name (2 letter code) [XX]:US
State or Province Name []:WA
Locality Name []:Seattle
Organization Name []:Friedlanders Hosiery
Organizational Unit Name []:IT
Common Name []:Friedlanders Hosiery Intermediate CA
Email Address []:
```

Finally, we can create the certificate for the Intermediate CA

```bash
$ cd /root/ca
$ openssl ca -config openssl.cnf -extensions v3_intermediate_ca \
      -days 3650 -notext -md sha256 \
      -in intermediate/csr/intermediate.csr.pem \
      -out intermediate/certs/intermediate.cert.pem

Enter pass phrase for ca.key.pem: secretpassword
Sign the certificate? [y/n]: y

$ chmod 444 intermediate/certs/intermediate.cert.pem
```

Now you should test the certificate with the following commands to ensure it was created correctly and that the trust chain between the Root and Intermediate CA's in in tact

```bash
$ openssl x509 -noout -text \
      -in intermediate/certs/intermediate.cert.pem

$ openssl verify -CAfile certs/ca.cert.pem \
      intermediate/certs/intermediate.cert.pem

intermediate.cert.pem: OK
```

### Signing Server Certificates

First we need to create a key for your chef server

```bash
$ cd /root/ca
$ openssl genrsa \
      -out intermediate/private/www.friedlandershosiery.com.key.pem 2048
$ chmod 400 intermediate/private/www.friedlandershosiery.com.key.pem
```

Next we create the CSR file

```bash
$ cd /root/ca
$ openssl req -config intermediate/openssl.cnf \
      -key intermediate/private/www.example.com.key.pem \
      -new -sha256 -out intermediate/csr/www.example.com.csr.pem

Enter pass phrase for www.example.com.key.pem: secretpassword
You are about to be asked to enter information that will be incorporated
into your certificate request.
-----
Country Name (2 letter code) [XX]:US
State or Province Name []:WA
Locality Name []:Seattle
Organization Name []:Friedlanders Hosiery
Organizational Unit Name []:IT
Common Name []:www.friedlandershosiery.com
Email Address []:webmaster@friedlandershosiery.com
```

Now we can create the server certificate we need

```bash
$ cd /root/ca
$ openssl ca -config intermediate/openssl.cnf \
      -extensions server_cert -days 375 -notext -md sha256 \
      -in intermediate/csr/www.friedlandershosiery.com.csr.pem \
      -out intermediate/certs/www.friedlandershosiery.com.cert.pem
$ chmod 444 intermediate/certs/www.friedlandershosiery.com.cert.pem
```

### Create a Chained Server Certificate File

The final step is to create a chain file that has the server certificate at the top followed by the certificate for the Intermediate CA. This step is important. Clients won't connect properly to the chef server if this step is omitted or done incorrectly. Set this file aside for the next steps

```bash
$ cd /root/ca
$ cat intermediate/certs/www.friedlandershosiery.com.cert.pem \
      intermediate/certs/intermediate.cert.pem > intermediate/certs/server-chain.cert.pem
$ chmod 444 intermediate/certs/server-chain.cert.pem
```

### Securing your Chef Server with your New Certificate Chain file

Now that we have a proper server certificate (chain) we will add that to our Chef Server and restart the server to incorporate it. Connect to your Chef Server from your chef workstation again.

[Reference]: https://docs.chef.io/server/server_security/

We need to create a Chef Server configuration file if you haven't already done so.

```bash
$ touch /etc/opscode/chef-server.rb
```

In that file add these lines.

```ruby
nginx['ssl_certificate'] = '/your/path/certs/server-chain.cert.pem'
nginx['ssl_certificate_key'] = '/your/path/www.friedlandershosiery.com.key.pem'
```

Save that file and now we need to update Chef Server.

```bash
$ sudo chef-server-ctl reconfigure
```

### Test a Connection to your Chef Server

If you open a browser instance and try to connect to your Chef Server now you should get the expected error that the certificate is not trusted.
