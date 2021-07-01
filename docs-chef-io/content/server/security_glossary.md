+++
title = "Security Glossary"

date = 2021-01-05T15:18:55-08:00
draft = true

[menu]
  [menu.server]
    title = "Security Glossary"
    identifier = "server/security/glossary"
    parent = "server/security/"
+++

## Authentication
  - User Authentication
  - Process Authentication

## TCP/IP

Chef Software follows the conceptual model of the internet based on the _internet protocol model_ and the _internet protocol suite_, which are commonly referred to as TCP/IP. This model specifies how data should be packaged, addressed, transmitted and recieved.

The TCP/IP model describes the architecture of the internet with five layers:

### Application Layer
  * Applications, or processes, create user data and communicate it to other applications on another or the same host.
  * The applications use services from the underlying layers, especially the transport layer which provides channels(pipes) to other processes.
  * Communication patterns are modeled by application architecture, for example, the client-server model and peer-to-peer networking.
  * The communication protocols for applications, such as SMTP, FTP, SSH, HTTP, operate on this level.
  * Processes are addressed through ports. Ports essentially represent services.

### Transport Layer
  * performs host-to-host communications on either the local network or remote networks separated by routers.[33]
  * It provides a channel for the communication needs of applications.
  * The _User Datagram Protocol (UDP)_ is the basic transport layer protocol, providing an unreliable connectionless datagram service. _Datagrams_ are the messages that applications send to each other on an _Internet Protocol_ (IP) network.
  * The _Transmission Control Protocol (TCP)_ provides flow-control, connection establishment, and reliable transmission of data.

### Network Layer
  exchanges datagrams across network boundaries.
  * It provides a uniform networking interface that hides the actual topology (layout) of the underlying network connections.
  * It is therefore also the layer that establishes internetworking.
  * Indeed, it defines and establishes the Internet.
  * This layer defines the addressing and routing structures used for the TCP/IP protocol suite.
  * The primary protocol in this scope is the Internet Protocol, which defines IP addresses.
  * Its function in routing is to transport datagrams to the next host, functioning as an IP router, that has the connectivity to a network closer to the final data destination.

### Datalink Layer defines the networking methods within the scope of the local network link on which hosts communicate without intervening routers.
  * This layer includes the protocols used to describe the local network topology and the interfaces needed to affect the transmission of Internet layer datagrams to next-neighbor hosts.

### The Physical Layer

A semi-OSI table

| Layer | Network Service Example |
|-------|-----------------|
| Application| DNS or HTTP (APIs operate on this level?)|
| Transport | TCP or UDP |
| Network | IP Layer 3 addresses added here|
| Data Link | Ethernet Layer 2 addresses added here |
| Physical | Bits sent over the network (packages) |

## TLS Security Protocol

The a typical client-to-server TLS Handshake has three stages in which the client and server:

1. negotiate an encrypted tunnel
1. agree on which TLS protocol to use
1. verify certificates

Negotiating the encrypted tunnel is a full round-trip between the client and server, in which the client sends a request to connect, or "synchronize" (`SYN`) with a server, then the server responds by agreeing to synchronize and acknowledging the request (`SYN ACK`), and the client then acknowledges the server's request (`ACK`).

Once the TCP connection is established, the client sends a message called a "ClientHello' which contains the maximum TLS version that the client can support, a random number ?, and a list of cyphersuites that it supports, and other TLS options. The server responds with a 'ServerHello' message that includes the TLS version that they will use, the cyphersuite that they will use and another(?) random number, and attatches its certificate including its public key? "Optionally, the server can also send a request for the client's certificate and parameters for other TLS extensions." (?)

Server then sends the server key exchange message, which includes the parameters for encryption, the server's digital certificate, which is the is encrypted with the server's private key, and then a 'ServerHelloDone' message to conclude the server's side of the communication.
The client then verifies the server's authenticity by decrypting the server's digital certificate using the server's public key.

The client then sends a change cypher spec message followed by a finished message that summarizes the entire handshake and is encrypted with the server's public key. The server decrypts the 'finished' message and responds with its own 'change cypher spec' message and a 'finished' message that also summarizes the entire handshake and ecrypts it with their private key. The two finish messages provide an additional piece of security, if the two finish messages don't match, then the client will cut off the connection.

The three way handshake provides a shared secret that provides encryption, authenticates the server, and proves that the exchange has not been tampered with through the 'finish' messages and the random numbers in the messages.

## Digital Signatures

## Certificates

### Certificate Authority

Acting as a certificate authority. see Custom CA

#### Trust

### Root Certificates

### Intermediate Certificates

### ??? Certificates

### Custom CA

## Encryption

## Keys
- Key Authentication
- Authentication using Shared Keys (Symmetric Keys)
- Authentication using Public Key Cryptography (Asymmetric Keys)

### Public Keys

Public Keys decrypt.

### Private Keys

Private keys encrypt.
## Tokens

## Chef Security Matrix

Chef Infra Server
Chef Infra Client
Chef Automate
Chef Habitat
Chef InSpec
