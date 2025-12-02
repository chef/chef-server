# OC Chef Pedant - Test Suite Documentation

## Table of Contents
1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Test Organization](#test-organization)
4. [Running Tests](#running-tests)
5. [Writing Tests](#writing-tests)
6. [Test Patterns](#test-patterns)
7. [API Test Categories](#api-test-categories)
8. [Configuration](#configuration)
9. [Troubleshooting](#troubleshooting)

## Overview

OC Chef Pedant is a comprehensive RSpec-based test suite for the Chef Server API. It provides end-to-end integration testing for all Chef Server REST API endpoints, ensuring proper functionality, authorization, and data validation.

### Purpose

- **API Validation**: Validates all Chef Server REST API endpoints
- **Authorization Testing**: Ensures proper access control and permissions
- **Data Integrity**: Verifies data persistence and retrieval
- **Regression Prevention**: Catches breaking changes in API behavior
- **Security Testing**: Validates authentication and authorization mechanisms

### Technology Stack

- **Language**: Ruby
- **Testing Framework**: RSpec
- **HTTP Client**: Net::HTTP with custom extensions
- **Configuration**: Ruby-based configuration files
- **Fixtures**: JSON payloads and test data in `fixtures/` directory

## Architecture

### Project Structure

```
oc-chef-pedant/
├── bin/
│   └── oc-chef-pedant          # Main test runner executable
├── fixtures/
│   ├── payloads/               # JSON test payloads for API requests
│   └── test_repository/        # Test cookbooks and data
├── lib/
│   ├── pedant/
│   │   ├── config.rb           # Configuration management
│   │   ├── platform.rb         # Platform abstraction
│   │   ├── requestor.rb        # HTTP request handling
│   │   ├── rspec/
│   │   │   ├── common.rb       # Common test helpers
│   │   │   └── matchers.rb     # Custom RSpec matchers
│   │   └── ...                 # Additional utilities
│   ├── pedant.rb               # Main library entry point
│   └── rspec-shared.rb         # Shared RSpec configurations
├── spec/
│   ├── api/                    # API endpoint tests
│   │   ├── account/            # Account management tests
│   │   ├── clients/            # Client endpoint tests
│   │   ├── cookbooks/          # Cookbook endpoint tests
│   │   ├── data_bag/           # Data bag endpoint tests
│   │   ├── environments/       # Environment endpoint tests
│   │   ├── keys/               # Key management tests
│   │   ├── nodes/              # Node endpoint tests
│   │   ├── policies/           # Policy endpoint tests
│   │   ├── roles/              # Role endpoint tests
│   │   ├── sandboxes/          # Sandbox endpoint tests
│   │   ├── search/             # Search endpoint tests
│   │   └── ...                 # Other endpoint tests
│   ├── org_creation/           # Organization creation tests
│   ├── running_configs/        # Runtime configuration examples
│   └── shared_context/         # Shared test contexts
├── pedant_config.rb            # Example configuration file
└── README.md                   # Basic setup and usage instructions
```

### Core Components

#### 1. Test Runner (`bin/oc-chef-pedant`)
- Command-line interface for running tests
- Supports filtering tests by tags, files, or patterns
- Configurable logging and output formats
- Integration with `chef-server-ctl test` command

#### 2. Configuration System (`lib/pedant/config.rb`)
- Environment-specific configuration
- Server endpoint configuration
- Authentication credentials
- Test user management
- Feature flags for optional tests

#### 3. Platform Abstraction (`lib/pedant/platform.rb`)
- Manages Chef Server communication
- Handles superuser and admin user operations
- Provides test user creation and cleanup
- Manages organization lifecycle

#### 4. Request Handling (`lib/pedant/requestor.rb`)
- HTTP request wrapper with authentication
- Automatic header management
- Request/response logging
- Error handling and retry logic

#### 5. Custom RSpec Matchers (`lib/pedant/rspec/matchers.rb`)
- `look_like` - Validates HTTP response structure
- `be_valid_json` - Validates JSON response format
- `have_status_code` - Checks HTTP status codes
- Custom matchers for Chef-specific validations

## Test Organization

### Test Categories

#### API Endpoint Tests (`spec/api/`)

Tests are organized by Chef Server resource type:

1. **Authentication & Authorization**
   - `auth_spec.rb` - General authentication tests
   - `authenticate_user_spec.rb` - User authentication flows
   - `groups_acl_spec.rb` - Access control list tests
   - `principal_spec.rb` - Principal authentication

2. **Resource Management**
   - `clients/` - API client management
   - `cookbooks/` - Cookbook upload, download, and versioning
   - `data_bag/` - Data bag CRUD operations
   - `environments/` - Environment management
   - `nodes/` - Node registration and updates
   - `roles/` - Role definitions and assignments
   - `user_spec.rb` - User account management

3. **Advanced Features**
   - `policies/` - Policyfile support
   - `cookbook_artifacts/` - Cookbook artifact handling
   - `search/` - Search query validation
   - `sandboxes/` - File upload sandbox management
   - `keys/` - Public/private key management

4. **Organization & Account**
   - `organization_spec.rb` - Organization CRUD
   - `account/` - Account management endpoints

5. **Infrastructure**
   - `status_spec.rb` - Server health checks
   - `stats_spec.rb` - Statistics endpoints
   - `license_spec.rb` - License validation
   - `server_api_version_spec.rb` - API versioning

#### Organization Tests (`spec/org_creation/`)

Tests for organization lifecycle:
- Organization creation workflows
- Internal vs external organization APIs
- Multi-org authorization
- Org membership and permissions

#### Shared Contexts (`spec/shared_context/`)

Reusable test contexts and helpers:
- Common setup and teardown logic
- Shared test data
- Standard user and permission configurations

### Test Naming Conventions

Tests follow RSpec best practices:

```ruby
describe "Resource Name", :tag do
  context "HTTP_METHOD /endpoint/path" do
    context "with specific conditions" do
      it "should behave in expected way" do
        # Test implementation
      end
    end
  end
end
```

**Examples:**
- `describe "Cookbooks API endpoint", :cookbooks`
- `context "DELETE /cookbooks/<name>/<version>"`
- `context "as an authorized, normal user"`
- `it "should respond with 200 (\"OK\")"`

### Test Tags

Tests are tagged for selective execution:

- `:smoke` - Critical path smoke tests
- `:authorization` - Authorization-specific tests
- `:users` - User management tests
- `:cookbooks` - Cookbook-related tests
- `:search` - Search functionality tests
- `:policies` - Policy-related tests
- `:keys` - Key management tests
- `:focus` - Temporary tag for test development (should not be committed)

## Running Tests

### Via chef-server-ctl (Recommended)

```bash
# Run all default tests
chef-server-ctl test

# Run all tests including optional ones
chef-server-ctl test --all

# Run specific test file
chef-server-ctl test spec/api/cookbooks_spec.rb

# Run tests with specific tag
chef-server-ctl test --tag smoke

# Exclude internal organization tests
chef-server-ctl test --all --exclude-internal-orgs

# Run only internal organization tests
chef-server-ctl test --only-internal-orgs

# Run with verbose output
chef-server-ctl test --verbose
```

### Direct Execution

```bash
# From the oc-chef-pedant directory
bin/oc-chef-pedant -c pedant_config.rb

# With specific options
bin/oc-chef-pedant -c pedant_config.rb --tag smoke --verbose

# Run specific test file
bin/oc-chef-pedant -c pedant_config.rb spec/api/user_spec.rb

# Show all available options
bin/oc-chef-pedant -h
```

### Development Environment (DVM)

```bash
# Inside dev VM
cd /path/to/chef-server/oc-chef-pedant
dvm load oc-chef-pedant
dvm start oc-chef-pedant

# Run tests
chef-server-ctl test
```

### Test Configuration

Tests use configuration from:
1. **Production**: `/var/opt/opscode/oc-chef-pedant/etc/pedant_config.rb`
2. **Development**: Local `pedant_config.rb` file

Key configuration options:

```ruby
# Server endpoint
chef_server "https://localhost:443"

# Superuser credentials
superuser_name "pivotal"
# Set via environment variables:
# SUPERUSER_KEY, WEBUI_KEY, STATS_PASSWORD

# Test users
default_orgname "pedant-testorg"

# Feature flags
enable_ldap_testing false
run_internal_org_tests false

# Logging
log_file "http-traffic.log"
```

### Environment Variables

```bash
# Authentication keys
export SUPERUSER_KEY="/path/to/pivotal.pem"
export WEBUI_KEY="/path/to/webui.pem"
export STATS_PASSWORD="stats_password"

# Alternative with Chef Secrets
export CHEF_SECRET_CHEF-SERVER.SUPERUSER_KEY="$(cat /path/to/pivotal.pem)"
export CHEF_SECRET_CHEF-SERVER.WEBUI_KEY="$(cat /path/to/webui.pem)"
export CHEF_SECRET_OPSCODE_ERCHEF.STATS_PASSWORD="stats_password"
```

## Writing Tests

### Basic Test Structure

```ruby
require "pedant/rspec/common"

describe "My API Endpoint", :my_tag do
  let(:request_url) { "my/endpoint/path" }
  
  context "GET /my/endpoint" do
    let(:request_method) { :GET }
    let(:requestor) { admin_user }
    
    it "returns 200 OK" do
      get(request_url, requestor).should look_like({
        status: 200,
        body_exact: { "result" => "success" }
      })
    end
  end
end
```

### Using Common Helpers

#### Test Users

```ruby
# Predefined test users
admin_user           # Organization admin
normal_user          # Regular user with basic permissions
platform.superuser   # Server-level superuser (pivotal)
platform.admin_user  # Platform admin user
platform.bad_user    # User with minimal/no permissions
```

#### Request Methods

```ruby
# HTTP verb helpers
get(url, requestor)
post(url, requestor, payload)
put(url, requestor, payload)
delete(url, requestor)
patch(url, requestor, payload)

# Example
response = get("/organizations/pedant/nodes", admin_user)
```

#### Custom Matchers

```ruby
# look_like - Flexible response matcher
response.should look_like({
  status: 200,                    # Expected status code
  body: { "key" => "value" },     # Partial body match
  body_exact: { "exact" => "match" }  # Exact body match
})

# should_be_deleted - Verify resource deletion
delete(url, admin_user)
should_be_deleted

# should_not_be_deleted - Verify resource still exists
delete(url, unauthorized_user)
should_not_be_deleted
```

### Authorization Testing Pattern

```ruby
context "authorization" do
  let(:cookbook_name) { "test-cookbook" }
  let(:requestor) { normal_user }
  
  let(:restrict_permissions!) do
    restrict_permissions_to "/cookbooks/#{cookbook_name}",
      normal_user => ["read"],      # Grant read only
      admin_user => %w{read delete} # Grant read and delete
  end
  
  it "prevents unauthorized deletion", :authorization do
    restrict_permissions!
    delete(cookbook_url, normal_user).should look_like({
      status: 403,
      body: { "error" => "forbidden" }
    })
    should_not_be_deleted
  end
end
```

### Cookbook Testing Pattern

```ruby
require "pedant/rspec/cookbook_util"

describe "Cookbooks", :cookbooks do
  include Pedant::RSpec::CookbookUtil
  
  let(:cookbook_name) { "my-cookbook" }
  let(:cookbook_version) { "1.0.0" }
  
  before(:each) do
    make_cookbook(admin_user, cookbook_name, cookbook_version)
  end
  
  after(:each) do
    delete_cookbook(admin_user, cookbook_name, cookbook_version)
  end
  
  it "retrieves cookbook metadata" do
    get(named_cookbook_url, admin_user).should look_like({
      status: 200,
      body: {
        "name" => cookbook_name,
        "version" => cookbook_version
      }
    })
  end
end
```

### Search Testing Pattern

```ruby
describe "Search", :search do
  let(:search_url) { "search/node" }
  
  before(:each) do
    # Create test nodes
    create_node("node1", { "platform" => "ubuntu" })
    create_node("node2", { "platform" => "centos" })
    wait_for_index  # Ensure search index is updated
  end
  
  it "finds nodes by platform" do
    query = "platform:ubuntu"
    get("#{search_url}?q=#{CGI.escape(query)}", admin_user).should look_like({
      status: 200,
      body: {
        "total" => 1,
        "rows" => [{ "name" => "node1" }]
      }
    })
  end
end
```

### Data Validation Pattern

```ruby
describe "Input Validation" do
  context "with invalid data" do
    let(:invalid_payload) do
      {
        "name" => "invalid name with spaces",  # Invalid
        "version" => "not-semver"             # Invalid
      }
    end
    
    it "returns 400 Bad Request" do
      post(url, admin_user, invalid_payload).should look_like({
        status: 400,
        body: {
          "error" => ["Invalid name format", "Invalid version format"]
        }
      })
    end
  end
end
```

## Test Patterns

### Setup and Teardown

```ruby
describe "Resource Lifecycle" do
  # Runs once before all tests in this describe block
  before(:all) do
    @organization = create_test_organization
  end
  
  # Runs once after all tests in this describe block
  after(:all) do
    delete_test_organization(@organization)
  end
  
  # Runs before each individual test
  before(:each) do
    @test_data = generate_test_data
  end
  
  # Runs after each individual test
  after(:each) do
    cleanup_test_data(@test_data)
  end
end
```

### Shared Examples

```ruby
# Define shared example
shared_examples "successful resource creation" do
  it "returns 201 Created" do
    response.should look_like({
      status: 201,
      body: { "uri" => /\/resources\// }
    })
  end
  
  it "creates the resource" do
    get(resource_url, admin_user).should look_like({
      status: 200
    })
  end
end

# Use shared example
describe "Creating Resources" do
  context "with valid data" do
    include_examples "successful resource creation"
  end
end
```

### Let Variables

```ruby
describe "Lazy Evaluation" do
  # Lazily evaluated, cached within test
  let(:user) { create_test_user }
  
  # Eager evaluation, not cached
  let!(:node) { create_test_node }
  
  it "uses let variables" do
    expect(user.name).to eq("test-user")
  end
end
```

### Pending Tests

```ruby
# Mark test as pending
it "will be implemented later" do
  pending("Waiting for feature implementation")
  # Test code that doesn't work yet
end

# Skip test
xit "skipped test" do
  # This test will not run
end

# Skip entire context
xcontext "not ready yet" do
  # None of these tests will run
end
```

## API Test Categories

### 1. Authentication Tests (`spec/api/auth_spec.rb`)

Tests authentication mechanisms:
- API key validation
- User credential authentication
- Token-based authentication
- Session management

**Example:**
```ruby
it "authenticates with valid API key" do
  get("/organizations/#{org}/nodes", user_with_valid_key).should look_like({
    status: 200
  })
end

it "rejects invalid API key" do
  get("/organizations/#{org}/nodes", user_with_invalid_key).should look_like({
    status: 401,
    body: { "error" => "invalid_key" }
  })
end
```

### 2. Cookbook Tests (`spec/api/cookbooks/`)

Tests cookbook management:
- Upload and download
- Version management
- Dependency resolution
- File checksums
- Metadata validation

**Key Tests:**
- Cookbook creation with valid/invalid data
- Version constraints
- Cookbook deletion and cleanup
- Large cookbook handling
- Concurrent uploads

### 3. Node Tests (`spec/api/nodes/`)

Tests node management:
- Node registration
- Attribute updates
- Run list management
- Node search
- Node deletion

**Key Tests:**
- Node CRUD operations
- Automatic attribute merging
- Override attribute precedence
- Node environment association

### 4. Role Tests (`spec/api/roles/`)

Tests role functionality:
- Role creation and updates
- Run list composition
- Default and override attributes
- Role nesting

### 5. Environment Tests (`spec/api/environments/`)

Tests environment features:
- Environment CRUD
- Cookbook version constraints
- Default attributes
- Environment-specific searches

### 6. Data Bag Tests (`spec/api/data_bag/`)

Tests data bag operations:
- Data bag creation
- Item CRUD operations
- Data bag encryption (if configured)
- Large data bag handling

### 7. Search Tests (`spec/api/search/`)

Tests search functionality:
- Node search queries
- Role/environment searches
- Complex query syntax
- Partial attribute searching
- Search result pagination
- Index synchronization

**Example:**
```ruby
it "searches nodes by platform" do
  query = "platform:ubuntu AND chef_environment:production"
  get("search/node?q=#{CGI.escape(query)}", admin_user).should look_like({
    status: 200,
    body: {
      "start" => 0,
      "total" => Integer,
      "rows" => Array
    }
  })
end
```

### 8. Policy Tests (`spec/api/policies/`)

Tests Policyfile features:
- Policy creation and updates
- Policy groups
- Revision management
- Cookbook artifact associations

### 9. Key Management Tests (`spec/api/keys/`)

Tests public/private key handling:
- Key rotation
- Multiple key support
- Key validation
- Default key management

### 10. Organization Tests (`spec/api/organization_spec.rb`)

Tests organization management:
- Org creation (internal/external)
- Org member management
- Org deletion
- Multi-tenancy isolation

### 11. User Tests (`spec/api/user_spec.rb`)

Tests user account features:
- User CRUD operations
- Email validation
- External authentication (SAML/LDAP)
- Password management
- User key management

### 12. ACL Tests (`spec/api/groups_acl_spec.rb`)

Tests access control:
- Permission assignment
- Group-based permissions
- Permission inheritance
- Resource-level ACLs

### 13. Sandbox Tests (`spec/api/sandboxes/`)

Tests file upload sandbox:
- Sandbox creation
- File upload to sandbox
- Sandbox commit
- Sandbox cleanup

### 14. Status & Health Tests

Tests server health:
- `status_spec.rb` - Server status endpoint
- `stats_spec.rb` - Statistics collection
- `license_spec.rb` - License validation
- `server_api_version_spec.rb` - API version negotiation

## Configuration

### Basic Configuration (`pedant_config.rb`)

```ruby
# Server Configuration
chef_server "https://api.chef.io"

# Organization
default_orgname ENV["PEDANT_ORG"] || "pedant-testorg"

# Superuser
superuser_name "pivotal"

# Users
users = {
  admin: {
    name: "admin",
    create_me: true,
    associate: true
  },
  normal: {
    name: "normaluser",
    create_me: true,
    associate: true
  }
}

# Logging
log_file "/var/log/opscode/oc-chef-pedant/http-traffic.log"

# Feature Flags
enable_internal_org_tests false
enable_ldap_tests false

# Request Timeouts
request_timeout 30
```

### LDAP Testing Configuration

```ruby
# Enable LDAP testing (WARNING: Use only in test environments)
ldap_testing true

ldap({
  host: "ldap.example.com",
  port: 636,
  bind_dn: "cn=admin,dc=example,dc=com",
  bind_password: "password",
  base_dn: "dc=example,dc=com",
  login_attribute: "uid",
  test_user: {
    username: "testuser",
    password: "testpass"
  }
})
```

### Advanced Configuration

```ruby
# Custom Platform Configuration
class CustomPlatform < Pedant::Platform
  def custom_setup
    # Custom platform initialization
  end
end

# SSL Configuration
verify_ssl false  # For self-signed certificates in development

# Proxy Configuration
http_proxy "http://proxy.example.com:3128"

# Parallel Test Execution
parallel_test_workers 4

# Custom Request Headers
default_request_headers({
  "X-Custom-Header" => "value"
})
```

## Troubleshooting

### Common Issues

#### 1. Connection Refused

**Problem:** Tests fail with "Connection refused" error

**Solutions:**
```bash
# Verify Chef Server is running
chef-server-ctl status

# Check network connectivity
curl -k https://localhost/organizations/pedant-testorg

# Verify configuration
grep chef_server /var/opt/opscode/oc-chef-pedant/etc/pedant_config.rb
```

#### 2. Authentication Failures

**Problem:** Tests fail with 401 Unauthorized

**Solutions:**
```bash
# Verify superuser key
ls -la /etc/opscode/pivotal.pem

# Check environment variables
echo $SUPERUSER_KEY

# Regenerate keys if needed
chef-server-ctl user-key-edit pivotal -k /etc/opscode/pivotal.pem
```

#### 3. Test Timeouts

**Problem:** Tests hang or timeout

**Solutions:**
```ruby
# Increase timeout in pedant_config.rb
request_timeout 60  # Increase from default 30 seconds

# Check server logs for bottlenecks
tail -f /var/log/opscode/opscode-erchef/current
```

#### 4. Search Index Delays

**Problem:** Search tests fail intermittently

**Solutions:**
```ruby
# Add explicit wait for indexing
def wait_for_index(max_wait: 30)
  sleep 2
  retries = max_wait / 2
  retries.times do
    break if search_index_ready?
    sleep 2
  end
end
```

#### 5. Org Creation Failures

**Problem:** Organization creation tests fail

**Solutions:**
```bash
# Clean up stale orgs
chef-server-ctl org-list
chef-server-ctl org-delete pedant-testorg

# Verify database connectivity
chef-server-ctl status postgresql
```

### Debug Mode

Enable detailed logging:

```bash
# Run with debug output
chef-server-ctl test --log-level debug

# Or in config
log_level :debug
log_file "pedant-debug.log"
```

### HTTP Traffic Logging

View detailed HTTP requests:

```bash
# Check HTTP traffic log
tail -f /var/log/opscode/oc-chef-pedant/http-traffic.log

# Or configure custom log location
log_file "/tmp/pedant-http.log"
```

### Test Isolation Issues

If tests interfere with each other:

```ruby
# Ensure proper cleanup
after(:each) do
  # Clean up test resources
  delete_test_cookbooks
  delete_test_nodes
  delete_test_users
end

# Use unique names
let(:cookbook_name) { "test-cookbook-#{SecureRandom.uuid}" }
```

### Focus Tag Warnings

```bash
# Check for uncommitted :focus tags
git grep -n ":focus"

# Remove focus tags
find spec -name "*.rb" -exec sed -i '' 's/:focus//g' {} +

# Use pre-commit hook (see README.md)
cd .git/hooks && ln -s ../../pre-commit.sh pre-commit
```

## Best Practices

### 1. Test Isolation
- Always clean up resources in `after` blocks
- Use unique resource names (consider UUIDs)
- Don't depend on test execution order
- Use `before(:each)` instead of `before(:all)` when possible

### 2. Meaningful Assertions
```ruby
# Good - Specific assertions
it "creates cookbook with correct metadata" do
  response.should look_like({
    status: 201,
    body: {
      "cookbook_name" => cookbook_name,
      "version" => cookbook_version,
      "frozen" => false
    }
  })
end

# Avoid - Vague assertions
it "works" do
  response.status.should eq(200)
end
```

### 3. Test Organization
- Group related tests in `context` blocks
- Use descriptive test names
- Follow "Arrange, Act, Assert" pattern
- Keep tests focused and single-purpose

### 4. Error Handling
```ruby
# Test both success and failure cases
context "with valid input" do
  it "succeeds" do
    # Test success path
  end
end

context "with invalid input" do
  it "returns appropriate error" do
    # Test error handling
  end
end
```

### 5. Performance
- Minimize expensive operations in `before(:all)`
- Reuse test data when possible
- Use tags to run subset of tests during development
- Parallelize independent tests

### 6. Documentation
- Add comments for complex test setups
- Document non-obvious test prerequisites
- Explain "why" not just "what"
- Keep examples up to date

## Contributing

When adding new tests:

1. **Follow existing patterns** - Study similar tests
2. **Add appropriate tags** - For selective test execution
3. **Include both positive and negative tests** - Success and error cases
4. **Test authorization** - Verify permission checks
5. **Clean up resources** - Proper teardown
6. **Document complex scenarios** - Add helpful comments
7. **Run full test suite** - Ensure no regressions
8. **Update this documentation** - When adding new test categories

## References

- [Chef Server API Documentation](https://docs.chef.io/server/api_chef_server/)
- [RSpec Documentation](https://rspec.info/documentation/)
- [Chef Server Architecture](https://docs.chef.io/server/)
- [oc-chef-pedant README](README.md)

---

**Document Version:** 1.0  
**Last Updated:** December 2, 2025  
**Maintained By:** Chef Engineering Team  

*This documentation was created with AI assistance following Progress AI policies.*
