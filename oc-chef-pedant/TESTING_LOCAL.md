# Testing oc-chef-pedant Locally

This document explains how to run oc-chef-pedant tests in a local development environment.

## Running Individual Spec Files

You can run individual spec files locally without a full Chef Server installation:

```bash
cd oc-chef-pedant
bundle exec rspec spec/api/some_spec.rb --require spec_helper
```

The `spec/spec_helper.rb` file provides minimal Pedant initialization that:

- Configures RSpec with Pedant's custom DSL extensions (`shared`, `should_respond_with`, etc.)
- Includes `Pedant::RSpec::Common` globally so all test helpers are available
- Mocks the platform to avoid needing a running Chef Server

### Dry Run (Check Syntax Without Execution)

To verify that specs load correctly without actually running them:

```bash
cd oc-chef-pedant
bundle exec rspec spec/ --require spec_helper --dry-run
```

### Running Specific Test Patterns

```bash
# Run all API tests
bundle exec rspec spec/api/ --require spec_helper

# Run tests matching a pattern
bundle exec rspec spec/api/ --require spec_helper -e "user authentication"

# Run with documentation format
bundle exec rspec spec/api/some_spec.rb --require spec_helper --format documentation
```

**Note**: Tests that make actual HTTP requests to Chef Server will fail without a running server. Local testing is useful for:

- Verifying code syntax and structure
- Checking that modules load correctly
- Running static analysis
- Confirming test organization and naming

## Running Full Test Suite Against a Chef Server

The complete test suite requires a running Chef Server and should be executed via:

```bash
# In a Chef Server development VM (recommended):
chef-server-ctl test

# Or directly with proper configuration:
bin/oc-chef-pedant -c /etc/opscode/oc-chef-pedant/pedant_config.rb
```

The `bin/oc-chef-pedant` script:

- Loads the full Pedant configuration
- Connects to a running Chef Server instance
- Executes the complete API test suite
- Generates detailed logs in `/var/log/opscode/oc-chef-pedant/`

## Development Workflow

For local development on oc-chef-pedant itself:

1. **Make your changes** to spec files or lib files
2. **Check syntax locally**:
   ```bash
   bundle exec rspec spec/your_spec.rb --require spec_helper --dry-run
   ```
3. **Test against Chef Server** using DVM or a test environment:
   ```bash
   chef-server-ctl test
   ```

## Troubleshooting

### "undefined method" errors when running locally

If you see errors about undefined methods like `shared`, `should_respond_with`, etc., make sure you're using the `--require spec_helper` flag. These methods are part of Pedant's RSpec extensions that are loaded by the spec_helper.

### Tests fail with connection errors

This is expected when running locally without a Chef Server. The spec_helper provides enough initialization to verify syntax and loading, but actual test execution requires a running Chef Server instance.
