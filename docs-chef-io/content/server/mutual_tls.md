+++
title = "Mutual TLS"

date = 2021-01-05T15:18:55-08:00
draft = false

[menu]
  [menu.server]
    title = "Mutual TLS"
    identifier = "server/Mutual TLS"
    parent = "server"
    weight = 10

+++

## Configuring Mutual TLS (mTLS) for Chef Server

Transport layer security (or “TLS”) provides a mechanism by which a client can be assured that it is talking to a server in a secure fashion. The server provides a certificate and public key which the client can use to encrypt and decrypt traffic to and from the server with.

But that only covers the server to client traffic - the client is able to verify the server but not the other way around. The client does not have to provide any type of identity details to the server. And that's fine, that's how HTTPS traffic mostly works.

What happens, though, if the server wants to ensure that it is talking to a known client that it can verify? In that case, the client needs to provide a certificate keypair that the server can then use to assure itself that it is talking to a known, valid client. This is what mTLS, or Mutual TLS does.

This document assumes you are going to configure your own CA infrastructure to secure your Chef server and users with. We are using OpenSSL.

In our example configuration, you will create a Root CA, an Intermediate CA, and then certificates for both your Chef Server and a user. Each additional user will need a unique certificate. All of this work should be executed from your chef workstation. It is considered a security best practice to use an Intermediate CA to create certificates from in order to isolate the Root CA.

### Creating the Root CA

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

## References

Use this to troubleshoot certificate issues. It's a great, short backgrounder

[Reference]: https://medium.com/@superseb/get-your-certificate-chain-right-4b117a9c0fce

If you have trouble getting your certificates to work before you enable Mutual TLS, try testing them out with this command line

```bash
$ openssl s_client -connect <your_chef_server_fqdn>:443 -showcerts
```

Example Root CA openssl.cnf file

```text
# OpenSSL root CA configuration file.
# Copy to `/root/ca/openssl.cnf`

[ ca ]
# `man ca`
default_ca = CA_default

[ CA_default ]
# Directory and file locations.
dir               = /root/ca
certs             = $dir/certs
crl_dir           = $dir/crl
new_certs_dir     = $dir/newcerts
database          = $dir/index.txt
serial            = $dir/serial
RANDFILE          = $dir/private/.rand

# The root key and root certificate.
private_key       = $dir/private/ca.key.pem
certificate       = $dir/certs/ca.cert.pem

# For certificate revocation lists.
crlnumber         = $dir/crlnumber
crl               = $dir/crl/ca.crl.pem
crl_extensions    = crl_ext
default_crl_days  = 30

# SHA-1 is deprecated, so use SHA-2 instead.
default_md        = sha256

name_opt          = ca_default
cert_opt          = ca_default
default_days      = 375
preserve          = no
policy            = policy_strict

[ policy_strict ]
# The root CA should only sign intermediate certificates that match.
# See the POLICY FORMAT section of `man ca`.
countryName             = match
stateOrProvinceName     = match
organizationName        = match
organizationalUnitName  = optional
commonName              = supplied
emailAddress            = optional

[ policy_loose ]
# Allow the intermediate CA to sign a more diverse range of certificates.
# See the POLICY FORMAT section of the `ca` man page.
countryName             = optional
stateOrProvinceName     = optional
localityName            = optional
organizationName        = optional
organizationalUnitName  = optional
commonName              = supplied
emailAddress            = optional

[ req ]
# Options for the `req` tool (`man req`).
default_bits        = 2048
distinguished_name  = req_distinguished_name
string_mask         = utf8only

# SHA-1 is deprecated, so use SHA-2 instead.
default_md          = sha256

# Extension to add when the -x509 option is used.
x509_extensions     = v3_ca

[ req_distinguished_name ]
# See <https://en.wikipedia.org/wiki/Certificate_signing_request>.
countryName                     = Country Name (2 letter code)
stateOrProvinceName             = State or Province Name
localityName                    = Locality Name
0.organizationName              = Organization Name
organizationalUnitName          = Organizational Unit Name
commonName                      = Common Name
emailAddress                    = Email Address

# Optionally, specify some defaults.
countryName_default             = YOUR_COUNTRY_GOES_HERE ex. US
stateOrProvinceName_default     = YOUR_STATE_GOES_HERE ex. WA
localityName_default            = YOUR_CITY_GOES_HERE ex. Seattle
0.organizationName_default      = YOUR_BUSINESS_NAME ex. Friedlanders Hosiery
organizationalUnitName_default  = YOUR_ORG_UNIT_NAME ex. Information Technology
emailAddress_default            = YOUR_EMAIL ex. webmaster@friedlandershosiery.com

[ v3_ca ]
# Extensions for a typical CA (`man x509v3_config`).
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
basicConstraints = critical, CA:true
keyUsage = critical, digitalSignature, cRLSign, keyCertSign

[ v3_intermediate_ca ]
# Extensions for a typical intermediate CA (`man x509v3_config`).
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
basicConstraints = critical, CA:true, pathlen:0
keyUsage = critical, digitalSignature, cRLSign, keyCertSign

[ usr_cert ]
# Extensions for client certificates (`man x509v3_config`).
basicConstraints = CA:FALSE
nsCertType = client, email
nsComment = "OpenSSL Generated Client Certificate"
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer
keyUsage = critical, nonRepudiation, digitalSignature, keyEncipherment
extendedKeyUsage = clientAuth, emailProtection

[ server_cert ]
# Extensions for server certificates (`man x509v3_config`).
basicConstraints = CA:FALSE
nsCertType = server
nsComment = "OpenSSL Generated Server Certificate"
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer:always
keyUsage = critical, digitalSignature, keyEncipherment
extendedKeyUsage = serverAuth

[ crl_ext ]
# Extension for CRLs (`man x509v3_config`).
authorityKeyIdentifier=keyid:always

[ ocsp ]
# Extension for OCSP signing certificates (`man ocsp`).
basicConstraints = CA:FALSE
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer
keyUsage = critical, digitalSignature
extendedKeyUsage = critical, OCSPSigning

```

Example Intermediate CA openssl.cnf

```
# OpenSSL intermediate CA configuration file.
# Copy to `/root/ca/intermediate/openssl.cnf`.

[ ca ]
# `man ca`
default_ca = CA_default

[ CA_default ]
# Directory and file locations.
dir               = /root/ca/intermediate
certs             = $dir/certs
crl_dir           = $dir/crl
new_certs_dir     = $dir/newcerts
database          = $dir/index.txt
serial            = $dir/serial
RANDFILE          = $dir/private/.rand

# The root key and root certificate.
private_key       = $dir/private/intermediate.key.pem
certificate       = $dir/certs/intermediate.cert.pem

# For certificate revocation lists.
crlnumber         = $dir/crlnumber
crl               = $dir/crl/intermediate.crl.pem
crl_extensions    = crl_ext
default_crl_days  = 30

# SHA-1 is deprecated, so use SHA-2 instead.
default_md        = sha256

name_opt          = ca_default
cert_opt          = ca_default
default_days      = 375
preserve          = no
policy            = policy_loose

email_in_dn       = no
rand_serial       = no

[ policy_strict ]
# The root CA should only sign intermediate certificates that match.
# See the POLICY FORMAT section of `man ca`.
countryName             = match
stateOrProvinceName     = match
organizationName        = match
organizationalUnitName  = optional
commonName              = supplied
emailAddress            = optional

[ policy_loose ]
# Allow the intermediate CA to sign a more diverse range of certificates.
# See the POLICY FORMAT section of the `ca` man page.
countryName             = optional
stateOrProvinceName     = optional
localityName            = optional
organizationName        = optional
organizationalUnitName  = optional
commonName              = supplied
emailAddress            = optional

[ req ]
# Options for the `req` tool (`man req`).
default_bits        = 2048
distinguished_name  = req_distinguished_name
string_mask         = utf8only

# SHA-1 is deprecated, so use SHA-2 instead.
default_md          = sha256

# Extension to add when the -x509 option is used.
x509_extensions     = v3_ca

[ req_distinguished_name ]
# See <https://en.wikipedia.org/wiki/Certificate_signing_request>.
countryName                     = Country Name (2 letter code)
stateOrProvinceName             = State or Province Name
localityName                    = Locality Name
0.organizationName              = Organization Name
organizationalUnitName          = Organizational Unit Name
commonName                      = Common Name
emailAddress                    = Email Address

# Optionally, specify some defaults.
countryName_default             = YOUR_COUNTRY_GOES_HERE ex. US
stateOrProvinceName_default     = YOUR_STATE_GOES_HERE ex. WA
localityName_default            = YOUR_CITY_GOES_HERE ex. Seattle
0.organizationName_default      = YOUR_BUSINESS_NAME ex. Friedlanders Hosiery
organizationalUnitName_default  = YOUR_ORG_UNIT_NAME ex. Information Technology
emailAddress_default            = YOUR_EMAIL ex. webmaster@friedlandershosiery.com

# Subject Alternative Names - required for locally created certs
[ alt_names ]
DNS.1 = www.friedlandershosiery.com
DNS.2 = friedlandershosiery.com
DNS.3 = chef.friedlandershosiery.com

[ v3_ca ]
# Extensions for a typical CA (`man x509v3_config`).
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
basicConstraints = critical, CA:true
keyUsage = critical, digitalSignature, cRLSign, keyCertSign

[ v3_intermediate_ca ]
# Extensions for a typical intermediate CA (`man x509v3_config`).
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
basicConstraints = critical, CA:true, pathlen:0
keyUsage = critical, digitalSignature, cRLSign, keyCertSign

[ usr_cert ]
# Extensions for client certificates (`man x509v3_config`).
basicConstraints = CA:FALSE
nsCertType = client, email
nsComment = "OpenSSL Generated Client Certificate"
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer
keyUsage = critical, nonRepudiation, digitalSignature, keyEncipherment
extendedKeyUsage = clientAuth, emailProtection

[ server_cert ]
# Extensions for server certificates (`man x509v3_config`).
basicConstraints = CA:FALSE
nsCertType = server
nsComment = "OpenSSL Generated Server Certificate"
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer:always
keyUsage = critical, digitalSignature, keyEncipherment
extendedKeyUsage = serverAuth
subjectAltName = @alt_names

[ crl_ext ]
# Extension for CRLs (`man x509v3_config`).
authorityKeyIdentifier=keyid:always

[ ocsp ]
# Extension for OCSP signing certificates (`man ocsp`).
basicConstraints = CA:FALSE
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer
keyUsage = critical, digitalSignature
extendedKeyUsage = critical, OCSPSigning

```

