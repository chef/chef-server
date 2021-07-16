+++
title = "Mutual TLS Overview"

date = 2021-01-05T15:18:55-08:00
draft = true

[menu]
  [menu.server]
    title = "TLS and mTLS"
    identifier = "server/security/mutual_tls"
    parent = "server/security"
    weight = 10

+++

Prerequisites:

- Chef Workstation
- Chef Server

Transport layer security (TLS) is a security protocol for enabling private and secure communication between computers on a network. The TLS protocol was published in 1999 and it has replaced the Secure Sockets Layer (SSL) protocol. It is sometimes described as `TLS/SSL` and `ssl` is still used in configuration setting names, even when they refer to `tls` configurations. If you're maintaining networked devices from the last century, then you should continue to use your legacy SSL configuration.

The archetypal use of TLS is for providing private and secure communication between a client and a server on the internet. When you visit a website from your computer, your computer requests a connection with the server and the website server sends a certificate and public key to your computer. Your computer uses that information to verify the web server's identity and to engage in encrypted communication with the web server.

The authentication between the client and the server is one-directional in the standard TLS protocol. The client verifes the server, but not the other way around. The client does not provide any type of identity details to the server. And that's fine, that's how HTTPS traffic mostly works.

What happens, though, if the server wants to ensure that it is talking to a known client that it can verify? In that case, the client needs to provide a certificate keypair that the server can then use to assure itself that it is talking to a known, valid client. This is what mTLS, or Mutual TLS does.

This document assumes you are going to configure your own custom CA infrastructure to secure your Chef server and users with. We are using OpenSSL.

In our example configuration, you will create a Root CA, an Intermediate CA, and then certificates for both your Chef Server and a user. Each additional user will need a unique certificate. All of this work should be executed from your chef workstation. It is considered a security best practice to use an Intermediate CA to create certificates from in order to isolate the Root CA.

Overview of process and define

* Root CA
* Intermediate CA
* Certificates for Chef Server and user
