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

## Task Implementation Workflow

### IMPORTANT: All tasks must be prompt-based and interactive

When implementing any task, follow this complete workflow:

### 1. Jira Integration & Requirements Gathering
- **When a Jira ID is provided**: Use the MCP server to fetch Jira issue details
- Read and understand the story requirements thoroughly
- **IMMEDIATELY**: Update Jira ticket status to "In Development" to indicate work has started
- **Set Product Module**: Update Jira ticket to set the product module to "Courier" for proper categorization
  - **Field**: Use `customfield_10111` ("Product - Module")
  - **Format**: `{"customfield_10111": {"value": "Courier"}}`
  - **Tool**: `mcp_atlassian-mcp_editJiraIssue`
- Provide summary of requirements and ask for confirmation before proceeding

### 2. Analysis & Planning
- Analyze the codebase to understand impact areas
- Identify files that need modification
- Plan the implementation approach
- **Prompt**: "Analysis complete. Implementation plan: [summary]. Shall I proceed with implementation?"

### 3. Implementation
- Create/modify source code following established patterns
- Ensure all changes follow the coding standards and patterns outlined below
- **Never modify prohibited files** (build configs, CI files, etc. without explicit permission)
- **Prompt after each major change**: "Implemented [component]. Summary: [details]. Next step: [next action]. Continue?"

### 4. Unit Testing (MANDATORY)
- Create comprehensive unit test cases for all new functionality
- Ensure test coverage remains **> 85%** at all times
- Use EUnit for Erlang tests, RSpec for Ruby tests
- Run tests locally to verify functionality
- **Prompt**: "Unit tests implemented. Coverage verified > 85%. Shall I proceed with final testing?"

### 5. Final Validation
- Run complete test suite: `make all` for Erlang services, `bundle exec rspec` for Ruby
- Verify code style compliance: `make elvis` or `bundle exec rubocop`
- Ensure no regressions in existing functionality
- **Prompt**: "All tests passed. Code validated. Ready for PR creation?"

### 6. Pull Request Creation & AI Compliance
- **MANDATORY STEP**: After implementation and testing completion, create PR immediately
- **Use GitHub CLI** for all Git operations
- **CRITICAL**: Always use `get_terminal_output` to read command results before proceeding
- **Step 6.1**: Check git status and verify current branch: `git status`
- **Step 6.2**: Create branch using Jira ID as branch name (if not already created): `git checkout -b [JIRA_ID]`
- **Step 6.3**: Add and commit changes with descriptive messages including Jira reference: `git add .` then `git commit -m "[JIRA_ID] - Description"`
- **Step 6.4**: Push changes: `git push origin [JIRA_ID]` (use `git push -u origin [JIRA_ID]` for first push)
- **Step 6.5**: Create PR using GitHub CLI with HTML-formatted description including:
  - `<h3>Summary</h3>` - Brief description of changes
  - `<h3>Jira Ticket</h3>` - Link to Jira issue with AI assistance note
  - `<h3>Changes Made</h3>` - Detailed list of modifications
  - `<h3>Testing</h3>` - Test coverage and validation details
  - **REQUIRED**: "This work was completed with AI assistance following Progress AI policies"
- **Step 6.6**: Add `ai-assisted` label to PR (create label if doesn't exist)
- **Step 6.7**: IMMEDIATELY update Jira ticket custom field `customfield_11170` to "Yes"
- **Step 6.8**: Add comprehensive work summary comment to Jira ticket including:
  - Summary of implementation approach
  - Key changes made and files modified
  - Testing approach and coverage details
  - PR link and completion status
- **Step 6.9**: Verify both PR labels and Jira field updates succeeded
- **Use ~/.profile for authentication setup**
- **ALWAYS READ TERMINAL OUTPUT**: Use `get_terminal_output` after each command to verify success
- **Prompt**: "PR created successfully with AI compliance. Summary: [PR details]. All compliance requirements met?"

### 7. Final Summary & Continuation
- Provide complete summary of all work performed including PR creation
- Confirm all AI compliance requirements are met
- List any remaining tasks or follow-up actions
- **Always ask**: "Task completed with PR created. Do you want to continue with the next step or task?"

### AI Compliance Requirements

#### 1. AI-Assisted Development & Compliance
- ✅ Create PR with `ai-assisted` label (if label doesn't exist, create it with description "Work completed with AI assistance following Progress AI policies" and color "9A4DFF")
- ✅ Include "This work was completed with AI assistance following Progress AI policies" in PR description

#### 2. Jira Ticket Updates (MANDATORY)
- ✅ **IMMEDIATELY after PR creation**: Update Jira ticket custom field `customfield_11170` ("Does this Work Include AI Assisted Code?") to "Yes"
- ✅ Use atlassian-mcp tools to update the Jira field programmatically
- ✅ **CRITICAL**: Use correct field format: `{"customfield_11170": {"value": "Yes"}}`
- ✅ Verify the field update was successful

#### 3. Documentation Requirements
- ✅ Reference AI assistance in commit messages where appropriate
- ✅ Document any AI-generated code patterns or approaches in PR description
- ✅ Maintain transparency about which parts were AI-assisted vs manual implementation

### Jira Field Specifications
To avoid confusion, here are the exact field specifications for mandatory Jira updates:

#### Product Module Field (customfield_10111)
- **Field Name**: "Product - Module"
- **Field Type**: `option-with-child` (cascading select)
- **Required Format**: `{"customfield_10111": {"value": "Courier"}}`
- **Usage**: Set during Step 1 (Jira Integration & Requirements Gathering)
- **Purpose**: Categorizes work under the Courier product module for proper project tracking

#### AI Assistance Field (customfield_11170)
- **Field Name**: "Does this Work Include AI Assisted Code?"
- **Field Type**: `option` (select)
- **Required Format**: `{"customfield_11170": {"value": "Yes"}}`
- **Usage**: Set during Step 6 (Pull Request Creation & AI Compliance)
- **Purpose**: Mandatory compliance field for Progress AI governance and audit trail

### Critical Requirements
- **Step 1**: IMMEDIATELY update Jira status to "In Development" when starting work
- **Step 1**: Set product module to "Courier" for proper project categorization
- **Never skip Step 6** - PR creation is mandatory after implementation
- **Never skip Jira field updates** - This is required for Progress AI governance
- **Always add work summary to Jira** - Document implementation approach and changes
- **Always verify updates succeeded** - Check response from atlassian-mcp tools
- **Treat as atomic operation** - PR creation and Jira updates should happen together
- **Double-check before final summary** - Confirm all AI compliance items are completed

### Interactive Development Rules

1. **All tasks are prompt-based** - Always provide status updates and ask for continuation
2. **Step-by-step progression** - Never skip ahead without user confirmation
3. **Clear summaries** - After each major step, summarize what was done and what's next
4. **Explicit continuation** - Always ask "Do you want to continue with the next step?"
5. **No autonomous changes** - All modifications require user awareness and approval
6. **Terminal Output Reading** - ALWAYS use `get_terminal_output` after running terminal commands to read and verify results before proceeding

### Terminal Usage Guidelines

- **CRITICAL**: Always use `get_terminal_output` tool after running any terminal command
- **Verify Success**: Read terminal output to confirm commands executed successfully
- **Error Handling**: If commands fail, read the error output and address issues before continuing
- **Command Sequence**: Never run multiple commands without reading output from previous commands
- **Authentication**: Use `~/.profile` for GitHub CLI and git authentication setup

### Audit Trail
All AI-assisted work must be traceable through:
1. GitHub PR labels (`ai-assisted`)
2. Jira custom field (`customfield_11170` = "Yes") 
3. PR descriptions mentioning AI assistance
4. Commit messages where relevant

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
    %% Setup code
    ok.

teardown(_) ->
    %% Cleanup code
    ok.

%% Test descriptions
user_creation_with_valid_data_succeeds_test() ->
    %% Test implementation
    ok.

user_creation_with_invalid_email_returns_validation_error_test() ->
    %% Test implementation
    ?assertEqual({error, invalid_email}, validate_email("not-an-email")).

%% Test data helpers
valid_user_data() ->
    #{name => <<"test_user">>, email => <<"test@example.com">>}.

invalid_user_data_email() ->
    (valid_user_data())#{email => <<"not-an-email">>}.
```

#### Mocking Best Practices
```erlang
%% Use meck for external dependencies
setup_mocks() ->
    meck:new(external_service, [non_strict]),
    meck:expect(external_service, call, fun(_) -> {ok, success} end).

teardown_mocks() ->
    meck:unload(external_service).

%% Verify mock interactions
test_with_mocks() ->
    setup_mocks(),
    try
        Result = my_module:function_that_calls_external_service(),
        ?assertEqual({ok, success}, Result),
        ?assert(meck:called(external_service, call, ['_']))
    after
        teardown_mocks()
    end.
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
  describe '#create_user' do
    context 'with valid user data' do
      it 'creates a new user successfully' do
        # Test implementation
      end
    end
    
    context 'with invalid email' do
      it 'returns validation error' do
        # Test implementation
      end
    end
  end
end

# Factory definitions
FactoryBot.define do
  factory :user do
    name { "Test User" }
    email { "test@example.com" }
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
    %% Setup test database
    Config.

end_per_suite(_Config) ->
    %% Cleanup test database
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Setup for each test
    Config.

test_user_creation_and_retrieval(_Config) ->
    %% Test database operations
    ok.
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
    %% Measure performance of user creation
    StartTime = os:timestamp(),
    Result = create_user(test_user_data()),
    EndTime = os:timestamp(),
    Duration = timer:now_diff(EndTime, StartTime),
    ?assert(Duration < 1000000), % Less than 1 second
    Result.
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
  let(:organization) { create(:organization) }
  let(:user) { create(:user, organization: organization) }
  let(:cookbook_data) do
    {
      name: 'apache2',
      version: '1.0.0',
      metadata: build(:cookbook_metadata),
      recipes: [build(:recipe)]
    }
  end
  
  it 'processes cookbook upload successfully' do
    service = described_class.new(organization)
    result = service.upload(cookbook_data, user)
    expect(result).to be_success
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
    %% Implementation details
    {ok, auth_token}.
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
  # Initializes the service with an organization context
  # @param organization [Organization] The organization for cookbook operations
  def initialize(organization)
    @organization = organization
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
    # User creation logic
  end
  
  def authenticate
    # Authentication logic
  end
  
  def send_email
    # Email sending logic
  end
  
  def generate_report
    # Reporting logic
  end
end

# Prefer: Single responsibility with service objects
class UserController
  def create
    service = UserCreationService.new(user_params)
    service.call
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
    "code": "ERROR_CODE",
    "message": "Human readable error message",
    "details": {
      "field": "specific_field",
      "reason": "Detailed reason for the error"
    }
  }
}
```

### Authentication & Authorization Patterns
```erlang
%% Always validate authentication first
case chef_authn:authenticate_request(Req) of
    {ok, #user{} = User} ->
        case chef_authz:is_authorized(User, Resource, Permission) of
            true -> process_request(Req, User);
            false -> {error, forbidden}
        end;
    {error, Reason} -> {error, {authentication_failed, Reason}}
end.
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
    [] ->
        Value = expensive_operation(Key),
        ets:insert(cache_table, {Key, Value, timestamp()}),
        Value;
    [{Key, Value, Timestamp}] ->
        case is_cache_expired(Timestamp) of
            true ->
                NewValue = expensive_operation(Key),
                ets:insert(cache_table, {Key, NewValue, timestamp()}),
                NewValue;
            false ->
                Value
        end
end.

%% Cache with TTL management
-spec cache_with_ttl(Key :: term(), Value :: term(), TTL :: integer()) -> ok.
cache_with_ttl(Key, Value, TTL) ->
    ExpiryTime = timestamp() + TTL,
    ets:insert(cache_table, {Key, Value, ExpiryTime}),
    ok.

%% Bulk cache operations for performance
-spec cache_bulk_data(Data :: [{term(), term()}], TTL :: integer()) -> ok.
cache_bulk_data(Data, TTL) ->
    ExpiryTime = timestamp() + TTL,
    CacheEntries = [{Key, Value, ExpiryTime} || {Key, Value} <- Data],
    ets:insert(cache_table, CacheEntries),
    ok.
```

### Memory Management
- Monitor memory usage in long-running Erlang processes
