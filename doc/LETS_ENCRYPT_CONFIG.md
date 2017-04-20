Here are the steps to get Let's Encrypt SSL certificates on Chef server

##Introduction

Let’s Encrypt is a free, automated, and open certificate authority (CA), run for the public’s benefit. 
How it works and technical details can be found on [https://letsencrypt.org](https://letsencrypt.org)

Let's Encrypt validation challenges are done via HTTP (port 80), TLS (port 443) or DNS.


##Prerequisites

1. Chef server must be reachable on port 80 and 443 on a public IP address. Port 80 is only required for Let's Encrypt validation challenges. 
2. A public fqdn configured and pointing to your Chef server IP address. For example: chef.example.com
3. [Certbot](https://certbot.eff.org/), recommended general purpose by Let's Encrypt client, or other  [Let's Encrypt ACME Client](https://letsencrypt.org/docs/client-options) of your choice.

##Configuration
The next steps can be performed with the [Certbot](https://certbot.eff.org/).

1. Follow the steps to get Certbot installed on the Chef server.
2. Edit /etc/opscode/cher-server.rb. As this server will face the internet directly, Chef server will run only on port 443 (HTTPS) to free port 80 be used only by Certbot standalone validation webserver. Therefore, we need to move Chef nginx non-ssl to another port, in this case 8080. 
```ruby
api_fqdn 'chef.example.com'
nginx['non_ssl_port']=8080
```
3. Reconfigure the Chef server
```bash
chef-server-ctl reconfigure
```
4. Execute certbot command to launch Certbot standalone webserver, perform the validation and create the certificate, where:

- **-d** is your Chef server public fqdn 
- **-m** your email address. 
- **--tls-sni-01-port 6443** makes Certbot binds TLS to another port as 443 is already being used by Chef nginx.
- **-preferred-challenges http** makes Certbot use the HTTP challenge.

```bash
certbot certonly --standalone -d chef.example.com -m your@email.com --tls-sni-01-port 6443 --preferred-challenges http
```
5. If sucessfull you should get the message:
```
   Congratulations! Your certificate and chain have been saved at
   /etc/letsencrypt/live/chef.example.com/fullchain.pem. Your
   cert will expire on yyyy-MM-dd. To obtain a new or tweaked version
   of this certificate in the future, simply run certbot again. To
   non-interactively renew *all* of your certificates, run "certbot
   renew"
```
6. Now, configure Chef to use the new certificates. Edit /etc/opscode/cher-server.rb to append with the SSL certificates 

```ruby
nginx['ssl_certificate']='/etc/letsencrypt/live/chef.example.com/fullchain.pem'
nginx['ssl_certificate_key']='/etc/letsencrypt/live/chef.example.com/privkey.pem'
```
7. Reconfigure the Chef server
```bash
chef-server-ctl reconfigure
```

8. At this point your Chef server is now using the newly created SSL certificates. You can browse the Chef server URL. For example: https://chef.example.com

9. A Let's Encrypt certificate is only valid for 90 days, therefore it requires to be renewed frequently. 
In this case, you can configure *cron* to run the certbot command below. By default, the certificate will only be renewed if it is valid for 30 days or less. 

```
certbot renew --renew-hook 'chef-server-ctl restart nginx'
``` 

 ##Alternatives

If you want to use a different utility to manage Let's Encrypt certificates, you might need to run a separate webserver o port 80 to perform the validation.
It is not recommended changing Chef nginx configuration for that.
