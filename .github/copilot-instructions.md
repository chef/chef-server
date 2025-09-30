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
- Use **lager** for logging with appropriate log levels (`debug`, `info`, `warning`, `error`)
- Implement proper error handling with pattern matching: `{ok, Result} | {error, Reason}`
- Follow Erlang naming conventions (snake_case for functions/variables, CamelCase for modules)
- Use appropriate OTP behaviors (gen_server, gen_statem, supervisor)
- Include comprehensive function documentation with `-spec` attributes
- Use meaningful variable names that describe the data being processed
- Avoid deeply nested case statements; prefer helper functions

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
    {error, Reason} ->
        ?LOG_ERROR("Failed to fetch user ~s: ~p", [UserId, Reason]),
        {error, database_error}
end
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
```ruby
# Use descriptive method and variable names
def authenticate_user_with_credentials(username, password)
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
def create_organization(name:, display_name:, owner:)
  # Implementation
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
- Achieve high test coverage (aim for >80%)
- Test both success and failure scenarios
- Use property-based testing where appropriate

### Integration Testing
- Use **oc-chef-pedant** for API integration tests
- Test service interactions thoroughly
- Include database integration tests

### Performance Testing
- Include performance benchmarks for critical paths
- Use **basho_bench** for load testing where applicable
- Monitor memory and CPU usage during tests

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

- Update relevant documentation for API changes
- Include code comments for complex logic
- Document configuration options and their effects
- Maintain up-to-date README files for each component

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
```

### Memory Management
- Monitor memory usage in long-running Erlang processes
- Use appropriate data structures for large datasets
- Implement garbage collection strategies where needed
- Profile memory usage during development

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
- **Release Notes**: Always update both CHANGELOG.md and the [Pending Release Notes](https://github.com/chef/chef-server/wiki/Pending-Release-Notes) wiki page for any significant changes, bug fixes, security updates, or new features

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
