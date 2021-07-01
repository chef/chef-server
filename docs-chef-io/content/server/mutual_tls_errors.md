+++
title = "Troubleshoot Mutual TLS"

date = 2021-01-05T15:18:55-08:00
draft = true

[menu]
  [menu.server]
    title = "Errors & Troubleshooting"
    identifier = "server/security/certs/mutual_tls_errors"
    parent = "server/security/certs"
    weight = 40

+++


## Use this to troubleshoot certificate issues. It's a great, short backgrounder

[Reference]: https://medium.com/@superseb/get-your-certificate-chain-right-4b117a9c0fce

## If you have trouble getting your certificates to work before you enable Mutual TLS, try testing them out with this command line

```bash
$ openssl s_client -connect <your_chef_server_fqdn>:443 -showcerts
```

