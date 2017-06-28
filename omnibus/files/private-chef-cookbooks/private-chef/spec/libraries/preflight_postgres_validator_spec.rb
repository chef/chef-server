require_relative '../../libraries/helper.rb'
require_relative '../../libraries/preflight_checks.rb'
require_relative '../../libraries/preflight_postgres_validator.rb'

class PG
  # dummy class for exception mocking, without requiring installation
  # of the PG gem and native extensions on Travis
  class ConnectionBad < RuntimeError; end
end

describe PostgresqlPreflightValidator do
  let(:node) do
    {
      'private_chef' => {
        'opscode-erchef' => { 'sql_user' => 'blah' },
        'oc_bifrost'     => { 'sql_user' => 'blah' },
        'oc_id'          => { 'sql_user' => 'blah' },
      }
    }
  end
  let(:first_run_response) { false }
  let(:secrets_exists_response) { false }
  let(:postgres_validator) {
    validator = PostgresqlPreflightValidator.new(node)

    # This lets us control the result of external_bootstrap_done?
    # by including let(:external_bootstrap_return) { true | false }
    # within each test or context.
    allow(validator)
      .to receive(:first_run?)
      .and_return(first_run_response)

    allow(validator)
      .to receive(:secrets_exists?)
      .and_return(secrets_exists_response)

    allow(validator)
      .to receive(:backend_verify_database_access)

    allow(validator)
      .to receive(:backend_verify_postgres_version)

    allow(validator)
      .to receive(:connect_as)
      .and_yield(Object.new)

    validator
  }

  context "#warn_about_removed_attribute" do
    before do
      allow(PrivateChef).to receive(:[]).with('postgresql').and_return(pg_attrs)
    end

    context "when the attribute is configured" do
      let(:pg_attrs) { {'foo' => 3000, 'bar' => true } }

      it "adds a warning to the ChefServer::Warnings" do
        expect(ChefServer::Warnings).to receive(:warn)
        postgres_validator.warn_about_removed_attribute('foo')
      end
    end

    context "when the attribute is not configured" do
      let(:pg_attrs) { { 'bar' => true } }

      it "adds a warning to the ChefServer::Warnings" do
        expect(ChefServer::Warnings).not_to receive(:warn)
        postgres_validator.warn_about_removed_attribute('foo')
      end
    end
  end

  context "#backend_verify_postgres_version" do
    # shadow other definition so we can call backend_verify_postgres_version
    let(:postgres_validator) { PostgresqlPreflightValidator.new(node) }

    let(:connection) { double('connection') }
    let(:version_reply) { [{ 'server_version' => version }] }
    let(:error_message) { /PostgreSQL version 9\.2 or greater/ }

    before do
      allow(connection).to receive(:exec).with('SHOW server_version;')
                                         .and_return(version_reply)
    end

    context "when external version is <= 8.x" do
      let(:version) { '8.100' }

      it "fails with a CSPG014 error" do
        expect(postgres_validator).to receive(:fail_with).with(error_message)
        postgres_validator.backend_verify_postgres_version(connection)
      end
    end

    context "when external version is <= 9.1" do
      let(:version) { '9.1' }

      it "fails with a CSPG014 error" do
        expect(postgres_validator).to receive(:fail_with).with(error_message)
        postgres_validator.backend_verify_postgres_version(connection)
      end
    end

    context "when external version is > 9.x" do
      let(:version) { '10.0' }

      it "fails with a CSPG014 error" do
        expect(postgres_validator).to receive(:fail_with).with(error_message)
        postgres_validator.backend_verify_postgres_version(connection)
      end
    end

    context "when external version is == 9.2" do
      let(:version) { '9.2' }

      it "does not fail with a CSPG014 error" do
        expect(postgres_validator).to_not receive(:fail_with)
        postgres_validator.backend_verify_postgres_version(connection)
      end
    end

    context "when external version is >= 9.2" do
      let(:version) { '9.6' }

      it "does not fail with a CSPG014 error" do
        expect(postgres_validator).to_not receive(:fail_with)
        postgres_validator.backend_verify_postgres_version(connection)
      end
    end
  end

  context "#connectivity_validation" do
    context "when a postgres exception is raised" do
      before do
        allow(postgres_validator).to receive(:connect_as).and_raise(pg_exception)
      end

      context "when the connection is refused" do
        let (:pg_exception) { PG::ConnectionBad.new("FATAL: could not connect to server: Connection refused") }

        it "fails with a CSPG010 error" do
          expect(postgres_validator).to receive(:err_CSPG010_postgres_not_available).and_return("cspg010 error")
          expect(postgres_validator).to receive(:fail_with).with("cspg010 error")

          postgres_validator.connectivity_validation
        end
      end

      context "when password authentication fails" do
        let (:pg_exception) { PG::ConnectionBad.new('FATAL: password authentication failed for user "fakeuser"') }

        it "does not raise an error" do
          expect(postgres_validator).to_not receive(:fail_with)
          expect { postgres_validator.connectivity_validation }.to_not raise_error
        end
      end

      context "when we connect but cannot find the role/database" do
        let (:pg_exception) { PG::ConnectionBad.new('FATAL: role "chef_server_conn_test" does not exist') }

        it "does not raise an error" do
          expect(postgres_validator).to_not receive(:fail_with)
          expect { postgres_validator.connectivity_validation }.to_not raise_error
        end
      end

      context "when we connect but cannot authenticate due to pg_hba settings" do
        let (:pg_exception) { PG::ConnectionBad.new('FATAL: no pg_hba.conf entry for host "1.2.3.4"') }

        it "does not raise an error" do
          expect(postgres_validator).to_not receive(:fail_with)
          expect { postgres_validator.connectivity_validation }.to_not raise_error
        end
      end

      context "when we receive an unexpected error" do
        let (:pg_exception) { PG::ConnectionBad.new("FATAL: something funky happened") }

        it "raises a CSPG999 error" do
          expect(postgres_validator).to receive(:fail_with).with("CSPG999: FATAL: something funky happened")
          postgres_validator.connectivity_validation
        end
      end
    end

    context "when a postgres exception is not raised" do
      it "does not raise an exception when called" do
        allow(postgres_validator).to receive(:connect_as)
        expect { postgres_validator.connectivity_validation }.to_not raise_error
      end
    end
  end

  context "#backend_validation" do
    context "when determining what it should validate" do
      describe "when this is the first run and secrets exist" do
        let (:first_run_response) { true }
        let (:secrets_exists_response) { true }
        it "does not check for existing databases or roles" do
          expect(postgres_validator).to_not receive(:backend_verify_named_db_not_present)
          expect(postgres_validator).to_not receive(:backend_verify_cs_roles_not_present)
          postgres_validator.backend_validation
        end
      end

      describe "when this is not the first run and secrets exist" do
        let (:first_run_response) { false }
        let (:secrets_exists_response) { true }
        it "does check for existing databases and roles" do
          expect(postgres_validator).to_not receive(:backend_verify_named_db_not_present)
          expect(postgres_validator).to_not receive(:backend_verify_cs_roles_not_present)
          postgres_validator.backend_validation
        end
      end

      describe "when this is the first run and no secrets exist" do
        let (:first_run_response) { true }
        let (:secrets_exists_response) { false }
        it "does check for existing databases and roles" do
          expect(postgres_validator)
            .to receive(:backend_verify_cs_roles_not_present)
              .at_least(:once)
          expect(postgres_validator)
            .to receive(:backend_verify_named_db_not_present)
              .at_least(:once)
          postgres_validator.backend_validation
        end
      end

      describe "when this is not the first run and no secrets exist" do
        let (:first_run_response) { false }
        let (:secrets_exists_response) { false }
        it "does not check for existing databases or roles" do
          expect(postgres_validator).to_not receive(:backend_verify_named_db_not_present)
          expect(postgres_validator).to_not receive(:backend_verify_cs_roles_not_present)
          postgres_validator.backend_validation
        end
      end
    end
  end

  context "#backend_verify_named_db_not_present" do
    it "throws an error if the database does exist" do
      allow(postgres_validator).to receive(:named_db_exists?).and_return true
      expect {postgres_validator.backend_verify_named_db_not_present(Object.new, "any_database")}
        .to raise_error PreflightValidationFailed, /CSPG016/
    end

    it "does not throw an error if the database does not exist" do
      allow(postgres_validator).to receive(:named_db_exists?).and_return false
      expect {postgres_validator.backend_verify_named_db_not_present(Object.new, "any_database")}
        .not_to raise_error
    end
  end

  context "#backend_verify_cs_roles_not_present" do
    it "throws an error if the role does exist" do
      expect(postgres_validator).to receive(:named_role_exists?)
      allow(postgres_validator).to receive(:named_role_exists?).and_return true
      expect {postgres_validator.backend_verify_cs_roles_not_present(Object.new)}.to raise_error PreflightValidationFailed, /CSPG017/
    end

    it "does not throw an error if the role does not exist" do
      allow(postgres_validator).to receive(:named_role_exists?).and_return false
      expect {postgres_validator.backend_verify_cs_roles_not_present(Object.new)}.not_to raise_error
    end
  end
end
