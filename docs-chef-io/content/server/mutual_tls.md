+++
title = "Mutual TLS Overview"

date = 2021-01-05T15:18:55-08:00
draft = true

[menu]
  [menu.server]
    title = "Mutual TLS"
    identifier = "server/security/mutual_tls"
    parent = "server/security"

+++

## Configuring Mutual TLS (mTLS) for Chef Server

Transport layer security (or "TLS") provides a mechanism by which a client can be assured that it is talking to a server in a secure fashion. The server provides a certificate and public key which the client can use to encrypt and decrypt traffic to and from the server with.

But that only covers the server to client traffic - the client is able to verify the server but not the other way around. The client does not have to provide any type of identity details to the server. And that's fine, that's how HTTPS traffic mostly works.

What happens, though, if the server wants to ensure that it is talking to a known client that it can verify? In that case, the client needs to provide a certificate keypair that the server can then use to assure itself that it is talking to a known, valid client. This is what mTLS, or Mutual TLS does.

This document assumes you are going to configure your own CA infrastructure to secure your Chef server and users with. We are using OpenSSL.

In our example configuration, you will create a Root CA, an Intermediate CA, and then certificates for both your Chef Server and a user. Each additional user will need a unique certificate. All of this work should be executed from your chef workstation. It is considered a security best practice to use an Intermediate CA to create certificates from in order to isolate the Root CA.

Overview of process and define
* Root CA
* Intermediate CA
* Certificates for Chef Server and user

