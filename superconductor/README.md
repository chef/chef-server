# SUPERCONDUCTOR #
This app updates the nginx configuration on our front-end load balancers
in response to HTTP requests.

## CONFIGURATION ##

### CONFIGURE SUPERCONDUCTOR ###
Configuration goes in `config/ENVIRONMENT_NAME.rb`. Look at
development.rb for an example.

## API ##

### INFORMATIONAL ENDPOINTS ###
* `GET /ping`: returns "PONG"
* `GET /status`: returns the JSON for {"ok" => "ok"}

### UPDATING THE NGINX CONFIGURATION ###
* `POST /configure`: POST body is JSON like `{"couchdb_orgs":
  ["orgname1", "orgname2", ...]}`  
  For example:
        require 'restclient'
        require 'json'
        a = ("aaa"..."zzz").map {|n| "longername-#{n}" }
        puts RestClient.post('http://localhost:9292/configure', {:couchdb_orgs => a}.to_json)
        # => {"ok":"ok"}

