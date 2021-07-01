+++
title = "Server Authentication"
draft = false

gh_repo = "chef-server"

[menu]
  [menu.server]
    title = "Server Authentication"
    identifier = "server/security/authentication"
    parent = "server/security"
    weight = 40
+++

Chef Infra Server uses the Chef Infra Server REST API to communicate with the Chef Workstation and the nodes that it manages. Chef Infra Server uses authentication to ensure that only trusted users have access to your infrastructure.

## Overview

The Chef Infra Server uses **public-key** cryptography, also called _asymmetric_ cryptography, for authentication with the Chef Infra Client.

You create your public and private key pair during your Chef Infra Client setup. The public key is stored on the Chef Infra Server, while the private key is returned to the user for safe keeping. (The private key is a `.pem` file located in the `.chef` directory or in `/etc/chef`.)

TLS Authentication

**Transport Layer Security** authentication is the process that clients and servers use to communicate securely over a network. TLS Authentication is required for servers and optional for clients. The prerequisites for TLS Authentication are:
* public/private keypairs on both the client and the server
* valid digital certificates

<img src="/images/server/TLS_authentication.svg" alt="Diagram of TLS Handshake" />

TCP handshake: Opens a communication tunnel between the client and server

TLS Authentication
1. Client Hello: maximum supported TLS version, supported cipher suites, random number
1. Server Hello: designates TLS version and cipher suite, random number. Sends a TLS Alert failure notification if the server doesn't support any of the client's TLS versions or cipher suites
1. Server Certificate: with the server public key attached to it.
1. Server Key Exchange: Parameters for key exchange and a digital signature. A digital signature is a set of the previous messages between summarized with the hash from the encryption suite and encrypted using the encrypted with the server's private key.
The client verifies that the server's certificate and decrypts the server signature with the server's public key sent in the Server Certificate Message
1. Server Hello Done: The server signals that it is finished with plain text messages

1. Client Key Exchange: (Can send signed certificate if you need to authenticate the clients) sends public value for Diffie-Hellman process. Server and client both have the same pre-master secret, which they combine with the random number. They use that to derive any key material.
1. Change cipher spec: Signals is the last unencrypted message.
1. Client Finished: All of the previous messages, summarized with the hash from the encryption suite and encrypted using the encrypted with the key that it has derived
1. Change cipher spec: Signals is the last unencrypted message.
1. ServerFinished: All of the previous messages, summarized with the hash from the encryption suite and encrypted using the encrypted with the key that it has derived

Produces:
- Shared secret to encrypt communication
- Authentication of the server
- Proof that the communication is secure by matching the finished messages and supplying random numbers in the 'Hello' messages.


TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256

TLS: Transport Layer Security
ECDHE: Elliptic Curfe Diffie-Hellman Encryption
RSA: Public Key encryption
AES: Cipher
128: Key size
GCM: Mode of operation (see aes explained video)
SHA256: Hash function

What Ciphers?
Secret key?
Authentication PK
Rubust : MITM, Replay, Downgrade

the Chef Infra Server API, which is a REST API that allows requests to
be made to the Chef Infra Server. Only authenticated requests will be
authorized. Most of the time, and especially when using knife, Chef
Infra Client, or the Chef Infra Server web interface, the use of the
Chef Infra Server API is transparent. In some cases, the use of the Chef
Infra Server API requires more detail, such as when making the request
in Ruby code, with a knife plugin, or when using cURL.

The authentication process ensures the Chef Infra Server responds only
to requests made by trusted users. Public key encryption is used by the
Chef Infra Server.

Both Chef Infra Client and `knife` use the Chef Infra Server API when
communicating with the Chef Infra Server. The `chef-validator` uses the
Chef Infra Server API, but only during the first Chef Infra Client run
on a node.

Each request to the Chef Infra Server from those executables sign a
special group of HTTP headers with the private key. The Chef Infra
Server then uses the public key to verify the headers and verify the
contents.

## Public and Private Keys

{{% security_chef_validator %}}

### Chef Infra Server Key Use

{{% chef_auth_authentication %}}

#### Chef Infra Client

Chef Infra Client authenticates with the Chef Infra Server using RSA
public key-pairs each time a Chef Infra Client needs access to data that
is stored on the Chef Infra Server. This prevents any node from
accessing data that it shouldn't and it ensures that only nodes that are
properly registered with the Chef Infra Server can be managed.

#### Knife

RSA public key-pairs are used to authenticate knife with the Chef Infra
Server every time knife attempts to access the Chef Infra Server. This
ensures that each instance of knife is properly registered with the Chef
Infra Server and that only trusted users can make changes to the data.

Knife can also use the `knife exec` subcommand to make specific,
authenticated requests to the Chef Infra Server. knife plugins can also
make authenticated requests to the Chef Infra Server by leveraging the
`knife exec` subcommand.

#### chef-validator

{{% security_chef_validator_context %}}

### Chef Infra Server Key Storage

Keys are stored in different locations, depending on if the location is
a node or a workstation.

#### Nodes

Each node stores its private key locally. This private key is generated
as part of the bootstrap process that initially installs Chef Infra
Client on the node. The first time Chef Infra Client runs on that node,
it uses the chef-validator to authenticate, but then on each subsequent
run it uses the private key generated for that client by the Chef Infra
Server.

#### Workstations

Each workstation stores its private key in the user's `~/.chef` directory.
This private key is generated by the Chef Infra Server and must be download
from the server and copied to the `~/.chef` directory manually. If you 
require a new private key, generate it with the Chef Infra Server and 
copy it to the `~/.chef` directory again.

{{% chef_repo_description %}}

{{% all_directory_chef %}}

### Chef Infra Server API Authentication

#### API Requests

{{% plugin_knife_summary %}}

{{% plugin_knife_using_authenticated_requests %}}

#### From the Web Interface

The Chef Infra Server user interface uses the Chef Infra Server API to
perform most operations. This ensures that authentication requests to
the Chef Infra Server are authorized. This authentication process is
handled automatically and is not something that users of the hosted Chef
Infra Server will need to manage. For the on-premises Chef Infra Server,
the authentication keys used by the web interface will need to be
maintained by the individual administrators who are responsible for
managing the server.

#### Other Options

The most common ways to interact with the Chef Infra Server using the
Chef Infra Server API abstract the API from the user. That said, the
Chef Infra Server API can be interacted with directly. The following
sections describe a few of the ways that are available for doing that.

**cURL**

An API request can be made using cURL, which is a Bash shell script that
requires two utilities: awk and openssl. The following example shows how
an authenticated request can be made using the Chef Infra Server API and
cURL:

```bash
#!/usr/bin/env bash

_chef_dir () {
  # Helper function:
  # Recursive function that searches for chef configuration directory
  # It looks upward from the cwd until it hits /.  If no directory is found,
  # ~/.chef is chosen if it exists
  # You could simply hard-code the path below

  if [ "$PWD" = "/" ]; then
  if [ -d ".chef" ]; then
    echo "/.chef"
      elif [ -d "$HOME/.chef" ]; then
        echo "$HOME/.chef"
      fi
    return
  fi

  if [ -d '.chef' ];then
    echo "${PWD}/.chef"
  else
    (cd ..; _chef_dir)
  fi
}

_chomp () {
  # helper function to remove newlines
  awk '{printf "%s", $0}'
}

chef_api_request() {
  # This is the meat-and-potatoes, or rice-and-vegetables, your preference really.

  local method path body timestamp chef_server_url client_name hashed_body hashed_path
  local canonical_request headers auth_headers

  chef_server_url="https://api.opscode.com/organizations/my_org"
  # '/organizations/ORG_NAME' is needed
  if echo $chef_server_url | grep -q "/organizations/" ; then
    endpoint=/organizations/${chef_server_url#*/organizations/}${2%%\?*}
  else
    endpoint=${2%%\?*}
  fi
  path=${chef_server_url}$2
  client_name="chef_user"
  method=$1
  body=$3

  hashed_path=$(echo -n "$endpoint" | openssl dgst -sha1 -binary | openssl enc -base64)
  hashed_body=$(echo -n "$body" | openssl dgst -sha1 -binary | openssl enc -base64)
  timestamp=$(date -u "+%Y-%m-%dT%H:%M:%SZ")

  canonical_request="Method:$method\nHashed Path:$hashed_path\nX-Ops-Content-Hash:$hashed_body\nX-Ops-Timestamp:$timestamp\nX-Ops-UserId:$client_name"
  headers="-H X-Ops-Timestamp:$timestamp \
    -H X-Ops-Userid:$client_name \
    -H X-Chef-Version:0.10.4 \
    -H Accept:application/json \
    -H X-Ops-Content-Hash:$hashed_body \
    -H X-Ops-Sign:version=1.0"

  auth_headers=$(printf "$canonical_request" | openssl rsautl -sign -inkey \
    "$(_chef_dir)/${client_name}.pem" | openssl enc -base64 | _chomp |  awk '{ll=int(length/60);i=0; \
    while (i<=ll) {printf " -H X-Ops-Authorization-%s:%s", i+1, substr($0,i*60+1,60);i=i+1}}')

  case $method in
    GET)
      curl_command="curl $headers $auth_headers $path"
      $curl_command
      ;;
    *)
      echo "Unknown Method. I only know: GET" >&2
      return 1
      ;;
    esac
  }

 chef_api_request "$@"
```

After saving this shell script to a file named `chef_api_request`, use
it similar to the following:

```bash
bash chef_api_request GET "/clients"
```

**Ruby**

On a system with Chef Infra Client installed, use Ruby to make an
authenticated request to the Chef Infra Server:

```ruby
require 'chef/config'
require 'chef/log'
require 'chef/rest'

chef_server_url = 'https://chefserver.com'
client_name = 'clientname'
signing_key_filename = '/path/to/pem/for/clientname'

rest = Chef::REST.new(chef_server_url, client_name, signing_key_filename)
puts rest.get_rest('/clients')
```

or:

```ruby
require 'mixlib/cli'
require 'chef'
require 'chef/node'
require 'chef/mixin/xml_escape'
require 'json'

config_file = 'c:/chef/client.rb'
Chef::Config.from_file(config_file)
Chef::Log.level = Chef::Config[:log_level]

def Usage()
  puts '/etc/chef/client.rb' # The config file location, e.g. ~/home/.chef/config.rb etc
  config_file = gets.chomp
  if (!File.exist?(config_file))
    puts 'config_file #{config_file} does not exist. Exiting.\n'
    exit
  end
  STDOUT.puts <<-EOF
    Choose options e.g. 1

    1 Display all nodes per environment
    2 Display all nodes in detail (can be slow if there a large number of nodes)
    9 Exit
  EOF
end

def ExecuteUserChoice()
  testoption = gets.chomp
  case testoption
  when '1'
    Execute(method(:DisplayNodesPerEnv))
  when '2'
    Execute(method(:DisplayNodesDetail))
  when '9'
    puts 'exit'
  else
    puts 'Unknown option #{testoption}. Exiting\n'
    exit
  end
end

def DisplayNodesPerEnv()
  Chef::Environment.list(false).each do |envr|
    print 'ENVIRONMENT: ', envr[0], '\n'
    Chef::Node.list_by_environment(envr[0], false).each do |node_info|
      print '\tNODE: ', node_info[0], '\n'
      print '\t\tURL: ', node_info[1], '\n'
    end
  end
end

def DisplayNodesDetail()
  Chef::Node.list(true).each do |node_array|
    node = node_array[1]
    print '#{node.name}\n'
    print '\t#{node['fqdn']}\n'
    print '\t#{node['kernel']['machine']}\n'
    print '\t#{node['kernel']['os']}\n'
    print '\t#{node['platform']}\n'
    print '\t#{node['platform_version']}\n'
    print '\t#{node.chef_environment}\n'
    print '\t#{node.run_list.roles}\n'
  end
end

def Execute(option)
  begin
    profilestart = Time.now
    option.call()
    profileend = Time.now
    timeofrun = profileend - profilestart
    print 'Time taken = #{timeofrun}'
  rescue Exception => ex
    print 'Error calling chef API'
    print ex.message
    print ex.backtrace.join('\n')
  end
end

Usage()
ExecuteUserChoice()
```

Another way Ruby can be used with the Chef Infra Server API is to get
objects from the Chef Infra Server, and then interact with the returned
data using Ruby methods. Whenever possible, the Chef Infra Server API
will return an object of the relevant type. The returned object is then
available to be called by other methods. For example, the `api.get`
method can be used to return a node named `foobar`, and then `.destroy`
can be used to delete that node:

```none
silly_node = api.get('/nodes/foobar')
silly_node.destroy
```

### Debug Authentication Issues

In some cases, Chef Infra Client may receive a 401 response to the
authentication request and a 403 response to an authorization request.
An authentication error error may look like the following:

```bash
[Wed, 05 Oct 2011 15:43:34 -0700] INFO: HTTP Request Returned 401
Unauthorized: Failed to authenticate as node_name. Ensure that your node_name and client key are correct.
```

To debug authentication problems, determine which Chef Infra Client is
attempting to authenticate. This is often found in the log messages for
that Chef Infra Client. Debug logging can be enabled on a Chef Infra
Client using the following command:

```bash
chef-client -l debug
```

When debug logging is enabled, a log entry will look like the following:

```bash
[Wed, 05 Oct 2011 22:05:35 +0000] DEBUG: Signing the request as NODE_NAME
```

If the authentication request occurs during the initial Chef Infra
Client run, the issue is most likely with the private key.

If the authentication is happening on the node, there are a number of
common causes:

-   The `client.pem` file is incorrect. This can be fixed by deleting
    the `client.pem` file and re-running Chef Infra Client. When Chef
    Infra Client re-runs, it will re-attempt to register with the Chef
    Infra Server and generate the correct key.
-   A `node_name` is different from the one used during the initial Chef
    Infra Client run. This can happen for a number of reasons. For
    example, if the client.rb file does not specify the correct node
    name and the host name has recently changed. This issue can be
    resolved by explicitly setting the node name in the client.rb file
    or by using the `-N` option for the Chef Infra Client executable.
-   The system clock has drifted from the actual time by more than 15
    minutes. This can be fixed by syncing the clock with an Network Time
    Protocol (NTP) server.

## Authorization

For more information about Chef Infra Server Authorization, see
[Organizations and Groups]({{< relref "server_orgs" >}}).

## Chef Infra Server API

For more information about using the Chef Infra Server API endpoints see
[Chef Infra Server API]({{< relref "api_chef_server" >}}).
