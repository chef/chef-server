## chef-server-ctl commands

This folder contains the additional commands that are added to chef-server-ctl for managing the server. These are commands for performing various admin related tasks on the chef-server box itself.

#### Testing

To run the unit tests for these commands, simply `cd` back to the base `chef-server` directory, then:

```
bundle install --binstubs
./bin/rake test:csc
```
