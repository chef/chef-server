# GitHub Copilot Instructions for Chef Server

This document provides comprehensive development guidelines and best practices for working on the Chef Server codebase. It helps GitHub Copilot provide more accurate and contextually appropriate suggestions while ensuring consistency across all team members and contributors.

## Quick Reference

### Essential Commands
```bash
# Development Environment
cd dev && vagrant up              # Start development VM
dvm load <service> && dvm start <service>  # Load and start service

# Building & Testing
make all                         # Erlang: clean, compile, eunit, dialyzer
rebar3 do clean, compile, ct     # Erlang: full test suite
bundle exec rspec                # Ruby: run RSpec tests
make ci                         # Run continuous integration suite

# Code Quality
./scripts/elvis rock            # Erlang style checking
bundle exec rubocop             # Ruby style checking (where applicable)
```

### File Patterns to Recognize
- `rebar.config` / `rebar.lock` → Erlang project dependencies
- `Gemfile` / `Gemfile.lock` → Ruby project dependencies  
- `*.erl` / `*.hrl` → Erlang source/header files
- `*_SUITE.erl` → Common Test suites
- `*_tests.erl` → EUnit test files
- `*_spec.rb` → RSpec test files

## Project Overview

Chef Server is a comprehensive infrastructure automation platform built as a distributed system with multiple services. The codebase is primarily written in **Erlang**, **Ruby**, and includes **Rails applications**, with build systems using **Omnibus** and **Habitat**.

## Architecture & Components

### Core Services (located in `src/`)

- **oc_erchef**: Core REST API server (Erlang/OTP)
- **bookshelf**: S3-compatible storage engine for cookbook data (Erlang/OTP)
- **oc_bifrost**: Authorization service (Erlang/OTP)
- **oc-id**: OAuth2 provider (Ruby on Rails)
- **chef-server-ctl**: Command-line management utility (Ruby)
- **nginx**: Web server with custom configurations

### Build & Packaging

- **omnibus/**: Omnibus-based packaging for production builds
- **habitat/**: Habitat-based containerization
- **dev/**: Vagrant-based development environment (DVM - Development Virtual Machine)

## Language-Specific Guidelines

### Erlang/OTP Development

#### Project Structure
- Use **rebar3** as the build tool for all Erlang projects
- Follow OTP application structure with proper supervision trees
- Each service has its own `rebar.config` and `rebar.lock` files

#### Code Standards

##### Formatting & Style
- **Line Length**: Maximum 120 characters per line
- **Indentation**: Use 4 spaces (no tabs) for consistency
- **Module Layout**: Follow standard OTP module structure:
  ```erlang
  %% Header comments and module documentation
  -module(module_name).
  
  %% Behaviors and includes
  -behaviour(gen_server).
  -include("module_name.hrl").
  
  %% Exports grouped by functionality
  %% API exports
  -export([start_link/0, stop/0]).
  %% gen_server callbacks
  -export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
  
  %% Type definitions
  -type state() :: #state{}.
  
  %% Macros and records
  -define(DEFAULT_TIMEOUT, 5000).
  ```

##### Naming Conventions
- **Functions/Variables**: `snake_case` (e.g., `process_request`, `UserId`)
- **Modules**: `snake_case` (e.g., `chef_server_api`, `user_manager`)
- **Atoms**: `snake_case` (e.g., `:ok`, `:error`, `:not_found`)
- **Constants/Macros**: `UPPER_SNAKE_CASE` (e.g., `?DEFAULT_TIMEOUT`)
- **Records**: `snake_case` with descriptive names (e.g., `#user_data{}`, `#request_context{}`)

##### Modern Language Features
- **Maps over Records**: Prefer maps for simple data structures, records for complex state
- **Binary Strings**: Use binary strings for text data: `<<"string">>`
- **List Comprehensions**: Use for data transformation when appropriate
- **Guards**: Utilize guards for input validation and optimization
- **Pattern Matching**: Leverage pattern matching for control flow and data extraction

##### Function Design
- Use **lager** for logging with appropriate log levels (`debug`, `info`, `warning`, `error`)
- Implement proper error handling with pattern matching: `{ok, Result} | {error, Reason}`
- Use appropriate OTP behaviors (gen_server, gen_statem, supervisor)
- Include comprehensive function documentation with `-spec` attributes
- Use meaningful variable names that describe the data being processed
- Avoid deeply nested case statements; prefer helper functions
- **Function Length**: Keep functions under 50 lines; extract complex logic into helper functions
- **Arity**: Prefer functions with fewer than 5 parameters; use records/maps for complex data

#### Logging Patterns
```erlang
%% Use lager macros for different log levels
?LOG_DEBUG("Processing request: ~p", [Request]),
?LOG_INFO("User ~s authenticated successfully", [Username]),
?LOG_ERROR("Database connection failed: ~p", [Error])

%% Include request IDs for tracing
?LOG_INFO("Request ~s: Operation completed", [ReqId])
```

#### Error Handling Patterns
```erlang
%% Prefer pattern matching over exceptions
case chef_db:fetch_user(UserId) of
    {ok, User} -> 
        process_user(User);
    {error, not_found} ->
        {error, user_not_found};
    {error, database_timeout} ->
        ?LOG_ERROR("Database timeout fetching user ~s", [UserId]),
        {error, service_unavailable};
    {error, Reason} ->
        ?LOG_ERROR("Failed to fetch user ~s: ~p", [UserId, Reason]),
        {error, database_error}
end

%% Use consistent error tuples with descriptive atoms
-spec validate_cookbook_data(Data :: map()) -> {ok, validated_data()} | {error, validation_error()}.
validate_cookbook_data(Data) ->
    case validate_required_fields(Data) of
        ok ->
            case validate_data_types(Data) of
                ok -> {ok, Data};
                {error, Reason} -> {error, {invalid_type, Reason}}
            end;
        {error, MissingFields} ->
            {error, {missing_fields, MissingFields}}
    end.

%% Implement retry logic for transient failures
retry_operation(Fun, MaxAttempts) ->
    retry_operation(Fun, MaxAttempts, 1).

retry_operation(_Fun, MaxAttempts, Attempt) when Attempt > MaxAttempts ->
    {error, max_retries_exceeded};
retry_operation(Fun, MaxAttempts, Attempt) ->
    case Fun() of
        {ok, Result} -> 
            {ok, Result};
        {error, transient_error} ->
            timer:sleep(exponential_backoff(Attempt)),
            retry_operation(Fun, MaxAttempts, Attempt + 1);
        {error, Reason} ->
            {error, Reason}
    end.
```

#### Dependencies
- Pin dependencies to specific commits/tags in `rebar.config`
- Keep `rebar.lock` files updated and committed
- Use Chef-maintained forks when available (e.g., `chef/sqerl`, `chef/pooler`)

#### Testing
- Write EUnit tests for unit testing with descriptive test names
- Use Common Test (CT) for integration tests
- Include proper test database setup in Makefiles
- Use meck for mocking external dependencies
- Test both success and failure scenarios
- Include property-based tests where appropriate using PropEr

#### Test Naming Conventions
```erlang
%% EUnit tests - descriptive names
user_creation_with_valid_data_succeeds_test() ->
    ?assertEqual({ok, user_created}, create_user(valid_user_data())).

invalid_email_format_returns_error_test() ->
    ?assertEqual({error, invalid_email}, validate_email("not-an-email")).
```

#### Build Commands
```bash
# Standard Erlang service build
make all                    # Clean, compile, eunit, dialyzer
rebar3 do clean, compile, eunit, dialyzer
rebar3 ct                   # Integration tests
rebar3 release              # Create release
```

### Ruby Development

#### Project Structure
- Use **Bundler** for dependency management with `bundle config set --local without development`
- Keep `Gemfile` and `Gemfile.lock` in sync and committed
- Separate development/test dependencies appropriately
- Use semantic versioning for gem dependencies
- Pin critical gems to specific versions

#### Rails Applications (oc-id)
- Follow Rails 7.x conventions and security best practices
- Use proper database migrations with rollback support
- Implement comprehensive request specs and controller tests
- Use FactoryBot for test data generation with realistic data
- Follow RESTful routing conventions
- Use strong parameters for input validation
- Implement proper error handling and user-friendly error messages

#### CLI Tools (chef-server-ctl)
- Use appropriate CLI frameworks (Thor, etc.)
- Implement comprehensive error handling with helpful error messages
- Include progress indicators for long-running operations
- Support both interactive and non-interactive modes
- Follow semantic versioning for gem dependencies
- Include comprehensive help text and examples

#### Testing Best Practices
- Use RSpec for testing Ruby code with descriptive test names
- Write comprehensive unit and integration tests
- Use proper test database isolation and cleanup
- Mock external dependencies appropriately
- Test edge cases and error conditions
- Use shared examples for common behavior
- Maintain fast test suites with parallel execution where possible

#### Ruby Code Style

##### Formatting & Style Guidelines
- **Line Length**: Maximum 120 characters per line
- **Indentation**: Use 2 spaces (no tabs) following Ruby community standards
- **String Literals**: Prefer single quotes unless interpolation is needed
- **Hash Syntax**: Use modern hash syntax `key: value` for symbol keys
- **Method Definitions**: Use explicit parentheses for method definitions with parameters

##### Class and Module Organization
```ruby
# Standard class structure
class ChefServerService
  # Constants first
  DEFAULT_TIMEOUT = 30.seconds
  
  # Include/extend statements
  include Logging
  extend Forwardable
  
  # Attribute accessors
  attr_reader :config, :client
  
  # Class methods
  def self.create(config)
    new(config).tap(&:initialize_client)
  end
  
  # Instance methods
  def initialize(config = {})
    @config = validate_config(config)
    @client = nil
  end
  
  private
  
  # Private methods at the end
  def validate_config(config)
    # Implementation
  end
end
```

##### Modern Ruby Features & Best Practices
- **Keyword Arguments**: Use for methods with multiple parameters
- **Safe Navigation**: Use `&.` operator for nil checking
- **Proc Shorthand**: Use `&:method` syntax when appropriate
- **String Interpolation**: Prefer over string concatenation
- **Enumerable Methods**: Use `map`, `select`, `reject` over manual loops
- **Exception Handling**: Use specific exception classes, avoid bare `rescue`

##### Method Design Standards
```ruby
# Use descriptive method and variable names
def authenticate_user_with_credentials(username:, password:)
  # Clear intent and proper error handling
  user = User.find_by(username: username)
  return { error: 'User not found' } unless user
  
  if user.authenticate(password)
    { success: true, user: user }
  else
    { error: 'Invalid credentials' }
  end
end

# Use keyword arguments for methods with multiple parameters
def create_organization(name:, display_name:, owner:, **options)
  validate_organization_params!(name, display_name, owner)
  
  Organization.create!(
    name: name,
    display_name: display_name,
    owner: owner,
    **options.slice(:description, :billing_plan)
  )
rescue ActiveRecord::RecordInvalid => e
  raise ValidationError, "Invalid organization data: #{e.message}"
end

# Prefer explicit returns for clarity in complex methods
def process_cookbook_upload(cookbook_data)
  return { error: 'Invalid cookbook data' } unless valid_cookbook?(cookbook_data)
  
  result = store_cookbook(cookbook_data)
  return { error: 'Storage failed' } unless result.success?
  
  { success: true, cookbook_id: result.id }
end
```

##### Error Handling Patterns
```ruby
# Define custom exception hierarchy
class ChefServerError < StandardError; end
class ValidationError < ChefServerError; end
class AuthenticationError < ChefServerError; end
class AuthorizationError < ChefServerError; end

# Use specific rescue blocks
def process_request(request)
  validate_request!(request)
  perform_operation(request)
rescue ValidationError => e
  log_error("Validation failed", error: e, request: request)
  { error: e.message, code: 'VALIDATION_ERROR' }
rescue AuthenticationError => e
  log_error("Authentication failed", error: e)
  { error: 'Authentication required', code: 'AUTH_ERROR' }
rescue StandardError => e
  log_error("Unexpected error", error: e, request: request)
  { error: 'Internal server error', code: 'INTERNAL_ERROR' }
end
```

### Configuration Management

#### Environment-Specific Configs
- Use `.erb` templates for dynamic configuration
- Separate development, test, and production configurations
- Environment variables should be documented

#### Database Configuration
- Support PostgreSQL as the primary database
- Include proper migration and schema management
- Use sqitch for database schema versioning

## Development Workflow

### Local Development Environment

#### Using DVM (Development Virtual Machine)
```bash
cd dev/
vagrant up                  # Start development VM
vagrant ssh                 # Access VM
sudo -i                     # Become root for dvm commands
dvm load <project_name>     # Load project for development
dvm start <project_name>    # Start service
```

#### Project Loading
- Use `dvm load` to set up development symlinks
- Services support hot-reloading for Erlang code changes
- Ruby services may require restarts for changes

### Build & Testing

#### Continuous Integration
- All tests must pass before merging
- Use `make ci` targets where available
- Run both unit tests and integration tests

#### Database Testing
- Use separate test databases
- Include proper setup/teardown in test suites
- Test database migrations thoroughly

### Code Quality Standards

#### Static Analysis
- Run **Dialyzer** for Erlang type checking
- Use **Elvis** for Erlang style checking
- Follow existing code formatting patterns

#### Error Handling
- Use proper error tuples in Erlang: `{ok, Result}` | `{error, Reason}`
- Implement comprehensive logging for debugging
- Handle database connection failures gracefully

## Specific Patterns & Conventions

### Database Access
- Use **sqerl** for SQL database interactions in Erlang
- Implement prepared statements for performance
- Use proper database connection retry logic

### HTTP/REST APIs
- Use **webmachine** framework for REST endpoints in Erlang
- Implement proper HTTP status codes and error responses
- Include comprehensive API documentation

### Authentication & Authorization
- Use **chef_authn** for authentication
- Implement proper authorization checks
- Support both internal and external authentication methods

### Logging & Monitoring
- Use structured logging with appropriate log levels
- Include request IDs for tracing
- Implement health check endpoints

## Testing Guidelines

### Unit Testing

#### Erlang Testing Standards
- **Test Organization**: Group related tests in modules with `_tests.erl` suffix
- **Test Naming**: Use descriptive names that explain the scenario and expected outcome
- **Coverage Goals**: Achieve >85% line coverage for critical modules
- **Test Data**: Use helper functions to generate consistent test data
- **Assertions**: Use appropriate EUnit assertions (`?assertEqual`, `?assertMatch`, `?assertError`)

#### Test Structure & Organization
```erlang
%% Test module structure
-module(user_manager_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test fixtures
setup() ->
    application:start(crypto),
    application:start(chef_db),
    ok.

teardown(_) ->
    application:stop(chef_db),
    application:stop(crypto).

%% Test descriptions
user_creation_with_valid_data_succeeds_test() ->
    User = valid_user_data(),
    ?assertEqual({ok, user_created}, user_manager:create_user(User)).

user_creation_with_invalid_email_returns_validation_error_test() ->
    User = invalid_user_data_email(),
    ?assertMatch({error, {validation_error, invalid_email}}, 
                 user_manager:create_user(User)).

%% Test data helpers
valid_user_data() ->
    #{username => <<"testuser">>,
      email => <<"test@example.com">>,
      password => <<"SecurePass123!">>}.

invalid_user_data_email() ->
    (valid_user_data())#{email => <<"not-an-email">>}.
```

#### Mocking Best Practices
```erlang
%% Use meck for external dependencies
setup_mocks() ->
    meck:new(chef_db, [unstick]),
    meck:expect(chef_db, fetch_user, fun(_) -> {ok, mock_user()} end),
    ok.

teardown_mocks() ->
    meck:unload(chef_db).

%% Verify mock interactions
test_with_mocks() ->
    setup_mocks(),
    Result = user_manager:authenticate_user(<<"testuser">>, <<"password">>),
    ?assert(meck:called(chef_db, fetch_user, ['_'])),
    ?assertEqual({ok, authenticated}, Result),
    teardown_mocks().
```

#### Ruby Testing Standards
- **RSpec Organization**: Use nested `describe` and `context` blocks for clarity
- **Test Naming**: Use descriptive strings that read like specifications
- **Test Data**: Use FactoryBot for complex object creation
- **Mocking**: Use RSpec mocks and stubs appropriately
- **Database Testing**: Use database_cleaner for proper test isolation

#### RSpec Test Structure
```ruby
# Test organization
RSpec.describe UserManager do
  describe '#authenticate_user' do
    context 'when user exists and password is correct' do
      let(:user) { create(:user, password: 'SecurePass123!') }
      
      it 'returns success with user data' do
        result = described_class.authenticate_user(user.username, 'SecurePass123!')
        
        expect(result).to be_success
        expect(result.user).to eq(user)
      end
    end
    
    context 'when user does not exist' do
      it 'returns authentication error' do
        result = described_class.authenticate_user('nonexistent', 'password')
        
        expect(result).to be_failure
        expect(result.error).to eq('user_not_found')
      end
    end
    
    context 'when password is incorrect' do
      let(:user) { create(:user, password: 'SecurePass123!') }
      
      it 'returns authentication error' do
        result = described_class.authenticate_user(user.username, 'wrong_password')
        
        expect(result).to be_failure
        expect(result.error).to eq('invalid_credentials')
      end
    end
  end
end

# Factory definitions
FactoryBot.define do
  factory :user do
    sequence(:username) { |n| "user#{n}" }
    sequence(:email) { |n| "user#{n}@example.com" }
    password { 'SecurePass123!' }
    
    trait :admin do
      admin { true }
    end
    
    trait :inactive do
      active { false }
    end
  end
end
```

### Integration Testing

#### Database Integration Tests
```erlang
%% Common Test suite for database integration
-module(user_db_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    application:start(chef_db),
    setup_test_database(),
    Config.

end_per_suite(_Config) ->
    cleanup_test_database(),
    application:stop(chef_db),
    ok.

init_per_testcase(_TestCase, Config) ->
    clean_database_tables(),
    Config.

test_user_creation_and_retrieval(_Config) ->
    UserData = #{username => <<"integration_test_user">>,
                 email => <<"test@integration.com">>},
    
    {ok, UserId} = chef_db:create_user(UserData),
    {ok, RetrievedUser} = chef_db:fetch_user(UserId),
    
    ?assertEqual(maps:get(username, UserData), 
                 maps:get(username, RetrievedUser)).
```

#### API Integration Tests
- Use **oc-chef-pedant** for comprehensive API testing
- Test authentication and authorization scenarios
- Validate HTTP status codes and response formats
- Test resource CRUD operations end-to-end
- Include performance benchmarks for critical endpoints

### Performance Testing

#### Load Testing Patterns
```erlang
%% Performance test using basho_bench patterns
performance_test_user_creation() ->
    StartTime = erlang:system_time(microsecond),
    
    Results = [create_test_user() || _ <- lists:seq(1, 1000)],
    
    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - StartTime,
    
    SuccessCount = length([ok || {ok, _} <- Results]),
    FailureCount = length(Results) - SuccessCount,
    
    ?assert(SuccessCount > 950), % 95% success rate
    ?assert(Duration < 10000000), % Under 10 seconds
    
    ct:log("Created ~p users in ~p microseconds (~p success, ~p failures)",
           [length(Results), Duration, SuccessCount, FailureCount]).
```

### Test Data Management

#### Test Environment Setup
```bash
# Test database setup
make test_db_setup          # Initialize test databases
make test_data_load         # Load test fixtures
make test_db_clean          # Clean test data between runs

# Test isolation
bundle exec rspec --order random  # Run tests in random order
bundle exec parallel_rspec spec/  # Parallel test execution
```

#### Test Data Factories
```ruby
# Complex test data scenarios
RSpec.describe CookbookUploadService do
  let(:organization) { create(:organization, :with_admin_user) }
  let(:cookbook_data) { build(:cookbook_data, :with_recipes, :with_metadata) }
  let(:uploader) { organization.admin_users.first }
  
  before do
    stub_external_storage_service
    setup_test_organization_permissions(organization)
  end
  
  it 'processes cookbook upload with all components' do
    result = described_class.new(organization).upload(cookbook_data, uploader)
    
    expect(result).to be_success
    expect(result.cookbook.version).to eq(cookbook_data.version)
    expect(result.cookbook.recipes.count).to eq(cookbook_data.recipes.count)
  end
end
```

### Test Coverage & Quality Gates

#### Coverage Requirements
- **Unit Tests**: Minimum 85% line coverage for core business logic
- **Integration Tests**: All API endpoints must have corresponding tests
- **Critical Paths**: 100% coverage for authentication, authorization, and data persistence
- **Edge Cases**: Test boundary conditions, error scenarios, and edge cases

#### Quality Metrics
```bash
# Coverage reporting
rebar3 cover                          # Erlang coverage
bundle exec rspec --format html      # Ruby coverage with SimpleCov

# Test performance monitoring
bundle exec rspec --profile 10       # Show slowest tests
rebar3 proper --numtests=1000        # Property-based testing
```

### Test Automation & CI Integration

#### Pre-commit Test Hooks
```bash
# Git hooks for test automation
#!/bin/bash
# .git/hooks/pre-commit

echo "Running tests before commit..."

# Erlang tests
make test || exit 1

# Ruby tests  
bundle exec rspec || exit 1

# Integration tests
make integration_test || exit 1

echo "All tests passed!"
```

#### Test Categories
1. **Unit Tests**: Fast, isolated tests for individual functions/classes
2. **Integration Tests**: Test component interactions and database operations
3. **API Tests**: End-to-end testing of REST endpoints with oc-chef-pedant
4. **Performance Tests**: Load testing and performance regression detection
5. **Security Tests**: Authentication, authorization, and vulnerability testing
6. **Contract Tests**: API contract validation and backward compatibility

## Build & Deployment

### Omnibus Builds
```bash
cd omnibus/
make dev                    # Set up development build environment
make dev-build              # Create package
```

### Habitat Builds
```bash
hab pkg build src/<service_name>   # Build individual service
./habitat_pkgs_build.sh            # Build all services
```

### Docker Compose
```bash
docker-compose up           # Start all services
docker-compose exec chef-server-ctl chef-server-test  # Run tests
```

## Security Considerations

- Never commit secrets or credentials
- Use proper SSL/TLS configuration
- Implement proper input validation
- Follow security best practices for web applications
- Use parameterized queries to prevent SQL injection

## Documentation Standards

### Code Documentation Guidelines
- **Module Documentation**: Include comprehensive module-level documentation explaining purpose and usage
- **Function Documentation**: Use proper `-spec` attributes for Erlang functions with clear parameter and return types
- **Complex Logic**: Document algorithmic complexity and design decisions
- **API Documentation**: Maintain up-to-date API documentation with examples
- **Configuration**: Document all configuration options and environment variables

#### Erlang Documentation Standards
```erlang
%% @doc This module handles user authentication and session management
%% for the Chef Server API. It provides functions for validating
%% user credentials and managing authentication tokens.
%% 
%% @author Chef Software Inc.
%% @since 1.0.0
-module(chef_authn).

%% @doc Authenticates a user with username and password.
%% This function validates user credentials against the configured
%% authentication backend and returns an authentication token on success.
%%
%% @param Username The username as a binary string
%% @param Password The password as a binary string
%% @returns {ok, AuthToken} on success, {error, Reason} on failure
%% @throws {auth_error, invalid_credentials} if credentials are invalid
%% @throws {auth_error, user_disabled} if user account is disabled
-spec authenticate_user(Username :: binary(), Password :: binary()) ->
    {ok, auth_token()} | {error, auth_error()}.
authenticate_user(Username, Password) ->
    %% Validate input parameters
    case validate_credentials(Username, Password) of
        ok ->
            %% Lookup user in database
            case user_db:find_by_username(Username) of
                {ok, User} ->
                    %% Verify password hash
                    verify_password_and_generate_token(User, Password);
                {error, not_found} ->
                    {error, {auth_error, user_not_found}}
            end;
        {error, Reason} ->
            {error, {validation_error, Reason}}
    end.
```

#### Ruby Documentation Standards
```ruby
# Service class for managing cookbook uploads and metadata processing.
# Handles validation, storage, and indexing of cookbook data within
# organizations while maintaining proper access controls.
#
# @example Basic usage
#   service = CookbookUploadService.new(organization)
#   result = service.upload(cookbook_data, user)
#   puts result.success? ? "Upload successful" : result.error
#
# @author Chef Software Inc.
# @since 2.0.0
class CookbookUploadService
  include Logging
  
  # Creates a new cookbook upload service for the specified organization
  #
  # @param organization [Organization] The organization context for uploads
  # @param options [Hash] Configuration options
  # @option options [Integer] :max_size Maximum cookbook size in bytes
  # @option options [Boolean] :validate_syntax Whether to validate Ruby syntax
  def initialize(organization, options = {})
    @organization = organization
    @options = default_options.merge(options)
    @validator = CookbookValidator.new(@options)
  end
  
  # Uploads and processes a cookbook within the organization context
  #
  # This method performs comprehensive validation of cookbook structure,
  # metadata, and content before storing in the configured backend.
  # It also updates search indexes and notifies relevant services.
  #
  # @param cookbook_data [Hash] The cookbook data structure
  # @param uploader [User] The user performing the upload
  # @return [UploadResult] Result object containing success/failure status
  # @raise [ValidationError] If cookbook data is invalid
  # @raise [AuthorizationError] If user lacks upload permissions
  # @raise [StorageError] If backend storage fails
  def upload(cookbook_data, uploader)
    validate_upload_permissions!(uploader)
    
    # Validate cookbook structure and content
    validation_result = @validator.validate(cookbook_data)
    return validation_result unless validation_result.success?
    
    # Process and store cookbook
    storage_result = store_cookbook(validation_result.processed_data)
    return storage_result unless storage_result.success?
    
    # Update search indexes and notify services
    update_indexes(storage_result.cookbook)
    notify_upload_completion(storage_result.cookbook, uploader)
    
    UploadResult.success(storage_result.cookbook)
  rescue StandardError => e
    log_error("Cookbook upload failed", error: e, cookbook: cookbook_data)
    UploadResult.failure(e.message)
  end
  
  private
  
  # Validates that the user has permission to upload cookbooks
  # @param user [User] The user to validate
  # @raise [AuthorizationError] If user lacks permissions
  def validate_upload_permissions!(user)
    unless @organization.user_can_upload?(user)
      raise AuthorizationError, "User #{user.username} cannot upload cookbooks"
    end
  end
end
```

### API Documentation Requirements
- **Endpoint Documentation**: Document all REST endpoints with request/response examples
- **Authentication**: Clearly document authentication requirements
- **Error Responses**: Document all possible error responses with status codes
- **Rate Limiting**: Document any rate limiting policies
- **Versioning**: Document API versioning strategy and deprecated endpoints

#### API Documentation Template
```ruby
# @api {post} /organizations/:org/cookbooks/:name/:version Upload Cookbook
# @apiName UploadCookbook
# @apiGroup Cookbooks
# @apiVersion 1.0.0
# 
# @apiDescription Uploads a new cookbook or updates an existing cookbook version.
# The cookbook data must be provided as a JSON payload with proper structure.
# 
# @apiParam {String} org Organization short name
# @apiParam {String} name Cookbook name (must match cookbook metadata)
# @apiParam {String} version Cookbook version (must be valid semver)
# 
# @apiHeader {String} Authorization Bearer token for authentication
# @apiHeader {String} Content-Type Must be application/json
# 
# @apiParamExample {json} Request Example:
# {
#   "cookbook_name": "apache2",
#   "version": "1.0.0",
#   "metadata": {
#     "description": "Apache HTTP Server cookbook",
#     "maintainer": "Chef Software"
#   },
#   "recipes": [
#     {
#       "name": "default.rb",
#       "content": "package 'apache2'"
#     }
#   ]
# }
# 
# @apiSuccess {String} cookbook_name Name of the uploaded cookbook
# @apiSuccess {String} version Version of the uploaded cookbook
# @apiSuccess {String} created_at ISO timestamp of creation
# 
# @apiSuccessExample {json} Success Response:
# HTTP/1.1 201 Created
# {
#   "cookbook_name": "apache2",
#   "version": "1.0.0",
#   "created_at": "2023-09-30T12:00:00Z"
# }
# 
# @apiError ValidationError Cookbook data validation failed
# @apiError AuthorizationError User lacks upload permissions
# @apiError ConflictError Cookbook version already exists
# 
# @apiErrorExample {json} Error Response:
# HTTP/1.1 400 Bad Request
# {
#   "error": {
#     "code": "VALIDATION_ERROR",
#     "message": "Invalid cookbook metadata",
#     "details": {
#       "field": "version",
#       "reason": "Must be valid semantic version"
#     }
#   }
# }
```

### README Documentation Standards
- **Project Overview**: Clear description of project purpose and scope
- **Installation Instructions**: Step-by-step setup guide
- **Configuration**: Document all configuration options
- **Usage Examples**: Provide practical examples
- **Contributing Guidelines**: Link to CONTRIBUTING.md
- **Support Information**: Contact and support channels

### Change Documentation
- **CHANGELOG.md**: Maintain detailed changelog following Keep a Changelog format
- **Migration Guides**: Document breaking changes and migration steps
- **Deprecation Notices**: Clearly communicate deprecated features
- **Version Compatibility**: Document version compatibility matrix

## Pull Request Guidelines

- Follow the CODE_REVIEW_CHECKLIST.md
- Include comprehensive commit messages
- Add tests for new functionality
- Update documentation as needed
- Ensure all CI checks pass
- Consider backward compatibility impacts

## Team Collaboration & Best Practices

## Team Collaboration & Best Practices

### Code Review Guidelines
- **Before Submitting**: Review the CODE_REVIEW_CHECKLIST.md
- **Commit Messages**: Use clear, descriptive commit messages following conventional commits
- **PR Size**: Keep pull requests focused and reasonably sized
- **Documentation**: Update relevant documentation for API changes
- **Testing**: Include tests for new functionality and bug fixes
- **Breaking Changes**: Clearly document any breaking changes and migration paths

#### Code Review Criteria
- **Code Quality**: Check for readability, maintainability, and adherence to style guides
- **Error Handling**: Verify comprehensive error handling and logging
- **Performance**: Review for potential performance bottlenecks
- **Security**: Check for security vulnerabilities and best practices
- **Testing**: Ensure adequate test coverage and quality
- **Documentation**: Verify that complex logic is documented

#### Common Anti-Patterns to Avoid
```erlang
%% Avoid: Deep nesting (pyramid of doom)
case validate_user(User) of
    {ok, ValidUser} ->
        case authenticate_user(ValidUser) of
            {ok, AuthUser} ->
                case authorize_user(AuthUser) of
                    {ok, AuthzUser} -> process_request(AuthzUser);
                    {error, Reason} -> {error, Reason}
                end;
            {error, Reason} -> {error, Reason}
        end;
    {error, Reason} -> {error, Reason}
end.

%% Prefer: Pipeline pattern with early returns
process_user_request(User) ->
    case validate_user(User) of
        {ok, ValidUser} -> authenticate_and_authorize(ValidUser);
        {error, Reason} -> {error, Reason}
    end.

authenticate_and_authorize(User) ->
    case authenticate_user(User) of
        {ok, AuthUser} -> authorize_and_process(AuthUser);
        {error, Reason} -> {error, Reason}
    end.
```

```ruby
# Avoid: God classes with too many responsibilities
class UserController
  def create
    # 100+ lines of user creation, validation, email sending, logging, etc.
  end
end

# Prefer: Single responsibility with service objects
class UserController
  def create
    result = UserCreationService.new(user_params).call
    
    if result.success?
      render json: result.user, status: :created
    else
      render json: { errors: result.errors }, status: :unprocessable_entity
    end
  end
end

class UserCreationService
  # Focused on user creation logic only
end
```

#### Quality Gates
- **Build Status**: All CI checks must pass
- **Test Coverage**: Minimum 85% for critical paths
- **Security Scan**: No high-severity vulnerabilities
- **Performance**: No significant performance regressions
- **Documentation**: API changes must include documentation updates

### Git Workflow
```bash
# Feature branch workflow
git checkout main
git pull origin main
git checkout -b feature/descriptive-name
# Make changes, commit frequently with clear messages
git push origin feature/descriptive-name
# Create PR, address review feedback
```

### Communication Patterns
- Use GitHub Issues for bug reports and feature requests
- Tag appropriate team members for reviews
- Use clear, descriptive PR titles and descriptions
- Include screenshots/examples for UI changes
- Reference related issues in PRs

## API Design Standards

### REST API Conventions
- Follow RESTful principles for resource endpoints
- Use appropriate HTTP status codes (200, 201, 400, 404, 500, etc.)
- Implement consistent error response formats
- Include API versioning strategies
- Document all endpoints with clear examples

### Error Response Format
```json
{
  "error": {
    "code": "RESOURCE_NOT_FOUND",
    "message": "The requested cookbook could not be found",
    "details": {
      "cookbook_name": "missing-cookbook",
      "version": "1.0.0"
    }
  }
}
```

### Authentication & Authorization Patterns
```erlang
%% Always validate authentication first
case chef_authn:authenticate_request(Req) of
    {ok, User} ->
        case chef_authz:is_authorized(User, Action, Resource) of
            true -> process_request(Req, User);
            false -> {error, forbidden}
        end;
    {error, Reason} ->
        {error, unauthorized}
end
```

## Performance & Scalability

### Database Optimization
- Use prepared statements for frequently executed queries
- Implement proper indexing strategies
- Use connection pooling (pooler) effectively
- Monitor query performance and optimize slow queries
- Implement proper pagination for large result sets

### Caching Strategies
```erlang
%% Cache frequently accessed data
case ets:lookup(cache_table, Key) of
    [{Key, Value, Timestamp}] ->
        case is_cache_valid(Timestamp) of
            true -> {ok, Value};
            false -> fetch_and_cache(Key)
        end;
    [] ->
        fetch_and_cache(Key)
end

%% Cache with TTL management
-spec cache_with_ttl(Key :: term(), Value :: term(), TTL :: integer()) -> ok.
cache_with_ttl(Key, Value, TTL) ->
    ExpiryTime = erlang:system_time(second) + TTL,
    ets:insert(cache_table, {Key, Value, ExpiryTime}),
    ok.

%% Bulk cache operations for performance
-spec cache_bulk_data(Data :: [{term(), term()}], TTL :: integer()) -> ok.
cache_bulk_data(Data, TTL) ->
    ExpiryTime = erlang:system_time(second) + TTL,
    CacheEntries = [{K, V, ExpiryTime} || {K, V} <- Data],
    ets:insert(cache_table, CacheEntries),
    ok.
```

### Memory Management
- Monitor memory usage in long-running Erlang processes
- Use appropriate data structures for large datasets
- Implement garbage collection strategies where needed
- Profile memory usage during development

#### Memory Optimization Patterns
```erlang
%% Use binary data for large text processing
-spec process_large_text(Text :: binary()) -> binary().
process_large_text(Text) when is_binary(Text) ->
    %% Process in chunks to avoid memory spikes
    ChunkSize = 64000, % 64KB chunks
    process_text_chunks(Text, ChunkSize, <<>>).

process_text_chunks(<<>>, _Size, Acc) ->
    Acc;
process_text_chunks(Text, Size, Acc) when byte_size(Text) =< Size ->
    ProcessedChunk = process_chunk(Text),
    <<Acc/binary, ProcessedChunk/binary>>;
process_text_chunks(Text, Size, Acc) ->
    <<Chunk:Size/binary, Rest/binary>> = Text,
    ProcessedChunk = process_chunk(Chunk),
    process_text_chunks(Rest, Size, <<Acc/binary, ProcessedChunk/binary>>).

%% Efficient list processing
-spec process_large_list(List :: [term()]) -> [term()].
process_large_list(List) ->
    %% Use tail recursion to avoid stack overflow
    process_list_tail(List, []).

process_list_tail([], Acc) ->
    lists:reverse(Acc);
process_list_tail([H|T], Acc) ->
    ProcessedItem = process_item(H),
    process_list_tail(T, [ProcessedItem|Acc]).
```

### Concurrency & Process Management
```erlang
%% Worker pool pattern for parallel processing
-spec parallel_process(Items :: [term()], WorkerCount :: integer()) -> [term()].
parallel_process(Items, WorkerCount) ->
    ChunkSize = max(1, length(Items) div WorkerCount),
    Chunks = chunk_list(Items, ChunkSize),
    
    Workers = [spawn_monitor(fun() -> process_chunk(Chunk) end) || Chunk <- Chunks],
    collect_results(Workers, []).

collect_results([], Results) ->
    lists:flatten(lists:reverse(Results));
collect_results([{Pid, Ref}|Rest], Results) ->
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            collect_results(Rest, Results);
        {'DOWN', Ref, process, Pid, {result, Result}} ->
            collect_results(Rest, [Result|Results]);
        {'DOWN', Ref, process, Pid, Error} ->
            ?LOG_ERROR("Worker process failed: ~p", [Error]),
            collect_results(Rest, Results)
    after 30000 ->
        ?LOG_ERROR("Worker timeout for process ~p", [Pid]),
        collect_results(Rest, Results)
    end.
```

### Ruby Performance Optimization
```ruby
# Efficient database queries with includes
class CookbookService
  def load_cookbooks_with_recipes(organization_id)
    # Avoid N+1 queries
    Cookbook.joins(:recipes)
            .includes(:recipes, :dependencies)
            .where(organization_id: organization_id)
            .order(:name)
  end
  
  # Batch processing for large datasets
  def process_large_cookbook_set(cookbooks)
    cookbooks.find_in_batches(batch_size: 1000) do |batch|
      process_cookbook_batch(batch)
    end
  end
  
  # Memory-efficient file processing
  def process_large_cookbook_file(file_path)
    File.open(file_path, 'r') do |file|
      file.each_line.lazy.each_slice(1000) do |lines|
        process_lines(lines)
      end
    end
  end
  
  private
  
  def process_cookbook_batch(batch)
    # Bulk operations instead of individual saves
    updates = batch.map { |cookbook| prepare_update(cookbook) }
    Cookbook.upsert_all(updates)
  end
end
```

### Performance Monitoring & Profiling
```erlang
%% Performance monitoring with timing
-spec timed_operation(Fun :: fun(), Context :: string()) -> {ok, term()} | {error, term()}.
timed_operation(Fun, Context) ->
    StartTime = erlang:system_time(microsecond),
    
    Result = try
        Fun()
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Operation ~s failed: ~p:~p", [Context, Class, Reason]),
            {error, {Class, Reason, Stacktrace}}
    end,
    
    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - StartTime,
    
    case Duration > 1000000 of % Log slow operations (>1s)
        true -> ?LOG_WARNING("Slow operation ~s took ~p microseconds", [Context, Duration]);
        false -> ?LOG_DEBUG("Operation ~s completed in ~p microseconds", [Context, Duration])
    end,
    
    Result.

%% Memory usage monitoring
-spec monitor_memory_usage(Process :: pid()) -> ok.
monitor_memory_usage(Process) ->
    {memory, Memory} = erlang:process_info(Process, memory),
    case Memory > 100000000 of % 100MB threshold
        true -> 
            ?LOG_WARNING("High memory usage in process ~p: ~p bytes", [Process, Memory]),
            garbage_collect(Process);
        false -> 
            ok
    end.
```

### Performance Testing & Benchmarking
```bash
# Performance testing commands
rebar3 proper --numtests=10000     # Property-based performance testing
basho_bench benchmarks/config     # Load testing with basho_bench

# Ruby performance testing
bundle exec ruby-prof script.rb   # Profile Ruby code
bundle exec benchmark-ips         # Benchmark Ruby methods
```

### Circuit Breaker & Rate Limiting
```erlang
%% Circuit breaker for external services
-record(circuit_breaker, {
    failures = 0 :: integer(),
    last_failure = undefined :: undefined | integer(),
    state = closed :: closed | open | half_open
}).

-spec call_with_circuit_breaker(Fun :: fun(), BreakerState :: #circuit_breaker{}) -> 
    {ok, term(), #circuit_breaker{}} | {error, circuit_open, #circuit_breaker{}}.
call_with_circuit_breaker(Fun, #circuit_breaker{state = open, last_failure = LastFailure} = Breaker) ->
    Now = erlang:system_time(second),
    case Now - LastFailure > 60 of % 60 second timeout
        true ->
            call_with_circuit_breaker(Fun, Breaker#circuit_breaker{state = half_open});
        false ->
            {error, circuit_open, Breaker}
    end;
call_with_circuit_breaker(Fun, Breaker) ->
    try
        Result = Fun(),
        {ok, Result, Breaker#circuit_breaker{failures = 0, state = closed}}
    catch
        _:_ ->
            NewFailures = Breaker#circuit_breaker.failures + 1,
            NewState = case NewFailures >= 5 of
                true -> open;
                false -> closed
            end,
            {error, external_service_failure, 
             Breaker#circuit_breaker{
                failures = NewFailures,
                last_failure = erlang:system_time(second),
                state = NewState
             }}
    end.
```

## Troubleshooting & Debugging

### Common Issues & Solutions

#### Erlang Services
```bash
# Check if service is running
ps aux | grep beam.smp

# Connect to running Erlang node
erl -name debug@127.0.0.1 -setcookie <cookie> -remsh <node>

# Check memory usage
observer:start().

# Enable debug logging
lager:set_loglevel(lager_console_backend, debug).
```

#### Database Issues
```bash
# Check database connections
chef-server-ctl psql opscode_chef -c "SELECT count(*) FROM pg_stat_activity;"

# Monitor slow queries
chef-server-ctl psql opscode_chef -c "SELECT query, mean_time FROM pg_stat_statements ORDER BY mean_time DESC LIMIT 10;"

# Database migration status
cd src/oc_erchef/schema && sqitch status
```

#### Performance Debugging
```bash
# Check system resources
top -p $(pgrep beam.smp)
iostat -x 1

# Monitor HTTP responses
tail -f /var/log/opscode/nginx/access.log | grep "5[0-9][0-9]"

# Database query monitoring
tail -f /var/log/opscode/postgresql/current | grep "LOG:"
```

### Logging Best Practices
```erlang
%% Include context in log messages
?LOG_INFO("Processing cookbook upload: ~s version ~s for org ~s", 
          [CookbookName, Version, OrgName]),

%% Use structured logging for metrics
?LOG_INFO("Request completed: method=~s path=~s status=~p duration=~pms", 
          [Method, Path, Status, Duration]),

%% Log errors with full context
?LOG_ERROR("Database operation failed: operation=~s error=~p context=~p", 
           [Operation, Error, Context])
```

## Environment-Specific Considerations

### Development Environment
- Use DVM for consistent development setup
- Keep development dependencies separate
- Use test databases for integration testing
- Enable debug logging for troubleshooting

### Production Considerations
- Monitor system metrics (CPU, memory, disk, network)
- Implement proper log rotation
- Use connection pooling for database access
- Configure appropriate timeouts for external services
- Implement circuit breakers for external dependencies

### Security Best Practices
- Never commit secrets or credentials to version control
- Use environment variables for sensitive configuration
- Implement proper SSL/TLS configuration
- Validate all user input thoroughly
- Use parameterized queries to prevent SQL injection
- Follow the principle of least privilege for service accounts
- Regularly update dependencies to patch security vulnerabilities

#### Input Validation Patterns
```erlang
%% Validate all user inputs before processing
-spec validate_cookbook_name(Name :: binary()) -> {ok, binary()} | {error, invalid_name}.
validate_cookbook_name(Name) when is_binary(Name) ->
    case re:run(Name, "^[a-zA-Z0-9_-]+$") of
        {match, _} when byte_size(Name) =< 255 -> {ok, Name};
        {match, _} -> {error, name_too_long};
        nomatch -> {error, invalid_characters}
    end;
validate_cookbook_name(_) ->
    {error, invalid_type}.

%% Sanitize SQL parameters
-spec safe_sql_query(Query :: string(), Params :: [term()]) -> {ok, term()} | {error, term()}.
safe_sql_query(Query, Params) ->
    case validate_sql_params(Params) of
        ok -> sqerl:select(Query, Params);
        {error, Reason} -> {error, {invalid_params, Reason}}
    end.
```

```ruby
# Strong parameter validation in Rails
class CookbooksController < ApplicationController
  private
  
  def cookbook_params
    params.require(:cookbook).permit(:name, :version, :description, metadata: {})
  end
  
  def validate_cookbook_name!(name)
    unless name.match?(/\A[a-zA-Z0-9_-]+\z/) && name.length <= 255
      raise ValidationError, "Invalid cookbook name: #{name}"
    end
  end
  
  # Prevent mass assignment vulnerabilities
  def safe_user_update
    user_params = params.require(:user).permit(:email, :display_name)
    # Never permit sensitive fields like :admin, :api_key directly
    current_user.update!(user_params)
  end
end
```

#### Authentication & Session Security
```erlang
%% Secure token generation
generate_api_token() ->
    Bytes = crypto:strong_rand_bytes(32),
    base64:encode(Bytes).

%% Constant-time password comparison to prevent timing attacks
-spec secure_compare(A :: binary(), B :: binary()) -> boolean().
secure_compare(A, B) when byte_size(A) =:= byte_size(B) ->
    crypto:hash_equals(A, B);
secure_compare(_, _) ->
    false.
```

```ruby
# Secure session management
class ApplicationController < ActionController::Base
  # Enable CSRF protection
  protect_from_forgery with: :exception
  
  # Secure session configuration
  before_action :configure_permitted_parameters, if: :devise_controller?
  before_action :validate_session_token
  
  private
  
  def validate_session_token
    if session[:user_id] && session[:token]
      unless secure_compare(session[:token], expected_token)
        reset_session
        redirect_to login_path, alert: 'Session invalid'
      end
    end
  end
  
  def secure_compare(a, b)
    return false unless a.bytesize == b.bytesize
    
    ActiveSupport::SecurityUtils.secure_compare(a, b)
  end
end
```

#### Data Protection & Encryption
```erlang
%% Encrypt sensitive data before storage
-spec encrypt_sensitive_data(Data :: binary(), Key :: binary()) -> binary().
encrypt_sensitive_data(Data, Key) ->
    IV = crypto:strong_rand_bytes(16),
    {ok, Ciphertext} = crypto:crypto_one_time(aes_256_cbc, Key, IV, Data, true),
    <<IV/binary, Ciphertext/binary>>.

-spec decrypt_sensitive_data(EncryptedData :: binary(), Key :: binary()) -> {ok, binary()} | {error, term()}.
decrypt_sensitive_data(<<IV:16/binary, Ciphertext/binary>>, Key) ->
    try
        {ok, crypto:crypto_one_time(aes_256_cbc, Key, IV, Ciphertext, false)}
    catch
        error:Reason -> {error, {decryption_failed, Reason}}
    end.
```

#### Authorization Patterns
```erlang
%% Role-based access control
-spec check_permission(User :: user(), Action :: atom(), Resource :: term()) -> boolean().
check_permission(User, Action, Resource) ->
    case get_user_permissions(User) of
        {ok, Permissions} ->
            case lists:member({Action, Resource}, Permissions) of
                true -> true;
                false -> check_inherited_permissions(User, Action, Resource)
            end;
        {error, _} -> false
    end.
```

#### Security Testing Requirements
- **Authentication Tests**: Verify all authentication mechanisms
- **Authorization Tests**: Test access controls for all resources
- **Input Validation Tests**: Test with malicious input patterns
- **Encryption Tests**: Verify data encryption/decryption workflows
- **Session Security Tests**: Test session management and timeout
- **Vulnerability Scanning**: Regular dependency vulnerability scans

## Maintenance & Operations

### Dependency Management
```bash
# Update Erlang dependencies
make bump_rebars

# Update Ruby dependencies  
bundle update

# Check for security vulnerabilities
bundle audit
```

### Database Maintenance
```bash
# Run database migrations
cd src/oc_erchef/schema && sqitch deploy

# Backup database
chef-server-ctl backup-data

# Restore database
chef-server-ctl restore-data backup.tar.gz
```

### Monitoring & Alerting
- Monitor service health endpoints
- Set up alerts for error rates and response times
- Monitor database performance metrics
- Track resource utilization trends
- Implement proper log aggregation and analysis

## Common Coding Patterns & Examples

### Erlang Service Module Pattern
```erlang
-module(my_service).
-behaviour(gen_server).

%% API
-export([start_link/0, process_request/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("my_service.hrl").

%% API Functions
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec process_request(Request :: term()) -> {ok, term()} | {error, term()}.
process_request(Request) ->
    gen_server:call(?MODULE, {process, Request}).

%% gen_server callbacks
init([]) ->
    ?LOG_INFO("Starting ~p", [?MODULE]),
    {ok, #state{}}.

handle_call({process, Request}, _From, State) ->
    case validate_request(Request) of
        {ok, ValidRequest} ->
            Result = do_process(ValidRequest),
            {reply, {ok, Result}, State};
        {error, Reason} ->
            ?LOG_ERROR("Invalid request: ~p", [Reason]),
            {reply, {error, invalid_request}, State}
    end.
```

### Ruby Service Class Pattern
```ruby
class ChefServerService
  include Logging
  
  def initialize(config = {})
    @config = config
    @client = setup_client
  end
  
  def process_request(request)
    validate_request!(request)
    
    result = with_retry(max_attempts: 3) do
      perform_operation(request)
    end
    
    log_success(request, result)
    result
  rescue ValidationError => e
    log_error("Request validation failed", error: e, request: request)
    raise
  rescue StandardError => e
    log_error("Unexpected error processing request", error: e, request: request)
    raise ServiceError, "Failed to process request"
  end
  
  private
  
  def validate_request!(request)
    raise ValidationError, "Request cannot be nil" if request.nil?
    # Additional validation logic
  end
  
  def perform_operation(request)
    # Core business logic
  end
end
```

### Database Access Pattern (Erlang)
```erlang
%% Using sqerl for database operations
-spec fetch_user_by_id(UserId :: binary()) -> {ok, user()} | {error, term()}.
fetch_user_by_id(UserId) ->
    case sqerl:select(fetch_user_by_id, [UserId]) of
        {ok, []} ->
            {error, not_found};
        {ok, [Row]} ->
            {ok, row_to_user(Row)};
        {error, Reason} ->
            ?LOG_ERROR("Database error fetching user ~s: ~p", [UserId, Reason]),
            {error, database_error}
    end.

%% Prepared statement in SQL file
%% -- name: fetch_user_by_id
%% SELECT id, username, email, created_at
%% FROM users
%% WHERE id = $1;
```

### Configuration Management Pattern
```erlang
%% Environment-specific configuration
get_config(Key) ->
    case application:get_env(my_app, Key) of
        {ok, Value} -> Value;
        undefined -> 
            ?LOG_WARNING("Missing configuration for ~p, using default", [Key]),
            get_default_config(Key)
    end.

get_default_config(database_pool_size) -> 10;
get_default_config(request_timeout) -> 30000;
get_default_config(_) -> undefined.
```

## Development Best Practices Summary

### Code Quality Checklist
- [ ] **Readability**: Code is self-documenting with clear variable and function names
- [ ] **Error Handling**: All error cases are handled appropriately
- [ ] **Logging**: Appropriate log levels with sufficient context
- [ ] **Testing**: Comprehensive tests covering happy path and edge cases
- [ ] **Documentation**: Complex logic is documented
- [ ] **Security**: No hardcoded secrets, proper input validation
- [ ] **Performance**: Efficient algorithms and data structures
- [ ] **Maintainability**: Code follows established patterns and conventions

### Pre-Commit Checklist
```bash
# Run before committing
make all                    # Build and test
./scripts/elvis rock        # Style check (Erlang)
bundle exec rubocop         # Style check (Ruby)
git add -A && git status    # Review changes
git commit -m "feat: descriptive commit message"
```

### Testing Strategy
1. **Unit Tests**: Test individual functions/modules in isolation
2. **Integration Tests**: Test service interactions and database operations  
3. **API Tests**: Test REST endpoints with oc-chef-pedant
4. **Performance Tests**: Benchmark critical paths
5. **Security Tests**: Validate authentication and authorization

### Documentation Standards
- **API Changes**: Update OpenAPI specs and documentation
- **Configuration**: Document new settings in README files
- **Architecture**: Update architecture diagrams for significant changes
- **Troubleshooting**: Add common issues to troubleshooting guides
- **Examples**: Provide working code examples for complex features

## Onboarding New Team Members

### Getting Started
1. **Repository Setup**: Clone repo, follow dev/README.md setup
2. **Development Environment**: Set up DVM with `vagrant up`
3. **Build Verification**: Ensure all services build successfully
4. **Test Execution**: Run test suites to verify environment
5. **Documentation Review**: Read architecture and API documentation

### Learning Path
1. **Week 1**: Understand architecture, set up development environment
2. **Week 2**: Make small bug fixes to learn codebase patterns
3. **Week 3**: Implement small features with mentorship
4. **Week 4**: Participate in code reviews and testing processes

### Mentorship Guidelines
- **Buddy System**: Assign experienced team member as mentor
- **Regular Check-ins**: Weekly one-on-ones for first month
- **Code Review Focus**: Provide detailed feedback on first PRs
- **Documentation**: Encourage questions and documentation improvements

## IDE and Tool Recommendations

### Recommended Extensions/Plugins
- **Erlang**: erlang-ls (Language Server), elvis (Linting)
- **Ruby**: Solargraph (Language Server), RuboCop (Linting)
- **Git**: GitLens for enhanced Git integration
- **Testing**: Test coverage visualization extensions
- **Database**: PostgreSQL client extensions

### Useful Tools
```bash
# Erlang tools
observer:start().          # GUI for process monitoring
recon:info(Pid).          # Process information
lager:set_loglevel(lager_console_backend, debug).

# Ruby tools  
pry                       # Interactive debugger
bundle exec rails console # Rails console
bundle audit              # Security vulnerability scanning

# System tools
htop                      # Process monitoring
iotop                     # I/O monitoring  
tcpdump                   # Network packet analysis
```

This comprehensive instruction set provides GitHub Copilot with deep context about Chef Server development practices, enabling it to generate more accurate, consistent, and helpful code suggestions for all team members regardless of their experience level with the codebase.
