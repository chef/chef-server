
The authentication process ensures that Chef Infra Server only responds to requests made by trusted users or clients. Chef Infra Server uses public key encryption. You create the public and private keys when you configure [Chef Infra Client](https://docs.chef.io/config_rb_client/) or setup [Chef Workstation](https://docs.chef.io/workstation/getting_started/#setup-chef-credentials).

* Chef Infra Server stores the public key
* Chef Workstation saves the private key in `~/.chef/`
* Chef Infra Client saves the private key in `/etc/chef`

Both Chef Infra Client and Chef Workstation communicate with the Chef Infra Server using the Chef Infra Server API. Each time that Chef Infra Client or Chef Workstation makes a request to Chef Infra Server, they use a special group of HTTP headers and sign the rest with their private key. The Chef Infra Server then uses the public key to verify the headers and the contents.
