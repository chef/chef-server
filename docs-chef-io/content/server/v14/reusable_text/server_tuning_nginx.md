The following settings are often modified from the default as part of
the tuning effort for the **nginx** service and to configure the Chef
Infra Server to use SSL certificates:

`nginx['ssl_certificate']`

:   The SSL certificate used to verify communication over HTTPS. Default
    value: `nil`.

`nginx['ssl_certificate_key']`

:   The certificate key used for SSL communication. Default value:
    `nil`.

`nginx['ssl_ciphers']`

:   The list of supported cipher suites that are used to establish a
    secure connection. To favor AES256 with ECDHE forward security, drop
    the `RC4-SHA:RC4-MD5:RC4:RSA` prefix. For example:

    ```ruby
    nginx['ssl_ciphers'] =  "HIGH:MEDIUM:!LOW:!kEDH: \
                             !aNULL:!ADH:!eNULL:!EXP: \
                             !SSLv2:!SEED:!CAMELLIA: \
                             !PSK"
    ```

`nginx['ssl_protocols']`

:   The SSL protocol versions that are enabled for the Chef Infra Server API.
    Starting with Chef Infra Server 14.3, this value defaults to `'TLSv1.2'` for
    enhanced security. Previous releases defaulted to `'TLSv1 TLSv1.1 TLSv1.2'`,
    which allowed for less secure SSL connections. TLS 1.2 is supported on
    Chef Infra Client 10.16.4 and later on Linux, Unix, and macOS, and on Chef 
    Infra Client 12.8 and later on Windows. If it is necessary to support these older end-of-life
    Chef Infra Client releases, set this value to `'TLSv1.1 TLSv1.2'`.

    ```ruby
    nginx['ssl_protocols'] = 'TLSv1.2'
    ```

<div class="admonition-note">

<p class="admonition-note-title">Note</p>

<div class="admonition-note-text">

See <https://www.openssl.org/docs/man1.0.2/man1/ciphers.html> for more
information about the values used with the `nginx['ssl_ciphers']` and
`nginx['ssl_protocols']` settings.

</div>

</div>

For example, after copying the SSL certificate files to the Chef Infra
Server, update the `nginx['ssl_certificate']` and
`nginx['ssl_certificate_key']` settings to specify the paths to those
files, and then (optionally) update the `nginx['ssl_ciphers']` and
`nginx['ssl_protocols']` settings to reflect the desired level of
hardness for the Chef Infra Server:

```ruby
nginx['ssl_certificate'] = '/etc/pki/tls/private/name.of.pem'
nginx['ssl_certificate_key'] = '/etc/pki/tls/private/name.of.key'
nginx['ssl_ciphers'] = 'HIGH:MEDIUM:!LOW:!kEDH:!aNULL:!ADH:!eNULL:!EXP:!SSLv2:!SEED:!CAMELLIA:!PSK'
nginx['ssl_protocols'] = 'TLSv1.2'
```
