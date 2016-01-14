require_relative '../../libraries/helper.rb'
require_relative '../../libraries/preflight_checks.rb'
require_relative '../../libraries/preflight_postgres_validator.rb'

describe PostgresqlPreflightValidator do
  let(:node) { { 'private_chef' => {'opscode-erchef' => { 'sql_user' => 'blah' }, 'oc_bifrost' => { 'sql_user' => 'blah' }, 'oc_id' => { 'sql_user' => 'blah' }} } }
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
