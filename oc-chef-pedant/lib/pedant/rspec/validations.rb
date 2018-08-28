# -*- coding: utf-8 -*-
# Copyright: Copyright 2012-2018 Chef Software, Inc.
# License: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

require 'securerandom'

module Pedant
  module RSpec
    module Validations
      module SharedMacros
        # Random Data Generators
        def random_text
          SecureRandom.hex(100)
        end

        def random_public_key
          OpenSSL::PKey::RSA.new(2048).public_key.to_s
        end

        # Acceptance / Rejection macros
        def rejects_with_400(options)
          let(:request_payload) { instance_eval_if_proc(options[:payload]) }
          let(:expected_response) { bad_request_exact_response }
          let(:error_message) { Array(instance_eval_if_proc(options[:error_message])) }

          if options[:skip_persistance_test]
            should_respond_with 400
          else
            should_respond_with 400, 'and not persist the resource' do
              begin
                persisted_resource_response.should look_like does_not_persist_response
              rescue URI::InvalidURIError
                # This can happen when we try to create an item with a
                # name that is illegal in a URL (like "this+ is
                # bad!!!").  In this case, we can assume the item was
                # not persisted :)
              end
            end
          end
        end

        def rejects_with_403(options)
          let(:request_payload) { instance_eval_if_proc(options[:payload]) }
          let(:expected_response) { forbidden_response }

          should_respond_with 403, 'and not persist the resource' do
            persisted_resource_response.should look_like does_not_persist_response
          end
        end

        def accepts_with_201(options, &additional_expectations)
          let(:request_payload) { instance_eval_if_proc(options[:payload]) }
          let(:expected_response) { created_response }

          should_respond_with 201, 'and persist the resource' do
            persisted_resource_response.should look_like ok_response
          end
        end

        def accepts_with_200(options, &additional_expectations)
          let(:request_payload) { instance_eval_if_proc(options[:payload]) }
          let(:expected_response) { ok_response }

          should_respond_with 200, 'and persist the resource' do
            persisted_resource_response.should look_like ok_response
          end
        end

        def validates_existence_of(attribute, options = {})
          context "without the \"#{attribute}\" attribute" do
            rejects_with_400 payload: Proc.new { default_resource_attributes.except(attribute) },
              error_message: ["Field '#{attribute}' missing"],
              skip_persistance_test: options[:skip_persistance_test]
          end
        end

        def rejects_invalid_value_of(attribute, options = {})
          context "when passing invalid value of \"#{options[:with]}\" to \"#{attribute}\" attribute", :validation do
            rejects_with_400 payload: Proc.new { default_resource_attributes.with(attribute, options[:with]) },
              error_message: options[:error_message] || options[:valid_format] || "Field '#{attribute}' invalid"
          end
        end

        def rejects_invalid_value(value, options = {})
          value = value || options[:with]
          context "when passing invalid value \"#{value}\"", :validation do
            rejects_with_400 payload: Proc.new { default_resource_attributes.with(validate_attribute, value) },
              error_message: options[:error_message] || options[:valid_format] || Proc.new { "Field '#{validate_attribute}' invalid" }
          end
        end

        def rejects_invalid_keys
          context 'with invalid top-level keys', :validation do
            rejects_invalid_value_of 'something_random', with: 'something random', error_message: 'Invalid key something_random in request body'
            rejects_invalid_value_of '漢字ひらがな한문', with: 'something random', error_message: 'Invalid key 漢字ひらがな한문 in request body'
            1.upto(3) do
              random_key = SecureRandom.hex(16)
              rejects_invalid_value_of random_key, with: 'something random', error_message: "Invalid key #{random_key} in request body"
            end
          end
        end

      end # SharedMacros

      # Update validations will look similar to this, and there may be some way to dry it up
      module Create
        extend ::Pedant::Concern

        included do
          let(:persisted_resource_response) { fail "Must define 'persisted_resource_response', usually a get() operation" }
          let(:does_not_persist_response) { resource_not_found_response }
          let(:resource_name) { request_payload['name'] }

          extend Pedant::RSpec::Validations::SharedMacros
        end

        module ClassMethods

          # This only generates validation of minimum length, for now
          def validates_length_of(attribute, options = {})
            random_value_of_length = ->(len) { SecureRandom.hex(len)[1..len] }

            context "when validating the length of #{attribute}" do
              let(:test_payload) { ->(len) { default_resource_attributes.with(attribute, random_value_of_length.(len)) } }

              if options[:min]
                context "with value below the minimum length of #{options[:min]}", :validation do
                  rejects_with_400 payload: Proc.new { test_payload.(options[:min] - 1) },
                    error_message: [ options[:error_message] ]
                end
                context "with value at the minimum length of #{options[:min]}" do
                  accepts_with_201 payload: Proc.new { test_payload.(options[:min]) }
                end
                context "with value above the minimum length of #{options[:min]}" do
                  accepts_with_201 payload: Proc.new { test_payload.(options[:min] + 1) }
                end
              end

              if options[:max]
                pending "with value below the maximum length of #{options[:max]}"
                pending "with value at the maximum length of #{options[:max]}"
                pending "with value above the maximum length of #{options[:max]}"
              end
            end
          end

          def accepts_valid_value_of(attribute, options = {})
            context "when passing valid value of \"#{options[:with]}\" to \"#{attribute}\" attribute" do
              accepts_with_201 payload: Proc.new { default_resource_attributes.with(attribute, options[:with]) }
            end
          end

          def accepts_valid_value(value, options = {})
            value = value || options[:with]
            context "when passing valid value \"#{value}\"" do
              accepts_with_201 payload: Proc.new { default_resource_attributes.with(validate_attribute, value) }
            end
          end

          # optionally_accepts 'field', with: 'value'
          def optionally_accepts(_attribute, options = {})

            context "with the \"#{_attribute}\" attribute" do
              accepts_with_201 payload: Proc.new { default_resource_attributes.with(_attribute, options[:with]) }
            end

            context "without the \"#{_attribute}\" attribute" do
              accepts_with_201 payload: Proc.new { default_resource_attributes.except(_attribute) } do
                expects(parsed_response[_attribute]).to eql options[:default]
              end
            end
          end

          def optionally_accepts_value(_value, options = {})
            _value = _value.nil? ? options[:with] : _value

            context "with a value of #{_value}" do
              accepts_with_201 payload: Proc.new { default_resource_attributes.with(validate_attribute, _value) }
            end

            context "without the attribute" do
              accepts_with_201 payload: Proc.new { default_resource_attributes.except(validate_attribute) } do
                expects(parsed_response[_attribute]).to eql options[:default]
              end
            end
          end
        end # Class Methods
      end # Create

      module Update
        extend ::Pedant::Concern

        included do
          let(:persisted_resource_response) { fail "Must define 'persisted_resource_response', usually a get() operation" }
          let(:does_not_persist_response) { ok_exact_response }
          let(:success_message) { original_resource_attributes }

          extend Pedant::RSpec::Validations::SharedMacros
        end

        module ClassMethods

          # This only generates validation of minimum length, for now
          def validates_length_of(attribute, options = {})
            random_value_of_length = ->(len) { SecureRandom.hex(len)[1..len] }

            context "when validating the length of #{attribute}" do
              let(:test_payload) { ->(len) { default_resource_attributes.with(attribute, random_value_of_length.(len)) } }

              if options[:min]
                context "with value below the minimum length of #{options[:min]}", :validation do
                  rejects_with_400 payload: Proc.new { test_payload.(options[:min] - 1) },
                    error_message: [ options[:error_message] ]
                end
                context "with value at the minimum length of #{options[:min]}" do
                  accepts_with_200 payload: Proc.new { test_payload.(options[:min]) }
                end
                context "with value above the minimum length of #{options[:min]}" do
                  accepts_with_200 payload: Proc.new { test_payload.(options[:min] + 1) }
                end
              end

              if options[:max]
                pending "with value below the maximum length of #{options[:max]}"
                pending "with value at the maximum length of #{options[:max]}"
                pending "with value above the maximum length of #{options[:max]}"
              end
            end
          end

          def accepts_valid_value_of(attribute, options = {})
            context "when passing valid value of \"#{options[:with]}\" to \"#{attribute}\" attribute" do
              accepts_with_200 payload: Proc.new { default_resource_attributes.with(attribute, options[:with]) }
            end
          end

          def accepts_valid_value(value, options = {})
            value = value || options[:with]
            context "when passing valid value \"#{value}\"" do
              accepts_with_200 payload: Proc.new { default_resource_attributes.with(validate_attribute, value) }
            end
          end


          def optionally_accepts(_attribute, options = {})
            context "with the \"#{_attribute}\" attribute" do
              accepts_with_200 payload: Proc.new { default_resource_attributes.with(_attribute, options[:with]) }
            end

            context "without the \"#{_attribute}\" attribute" do
              accepts_with_200 payload: Proc.new { default_resource_attributes.except(_attribute) } do
                expects(parsed_response[attribute]).to eql options[:default]
              end
            end
          end

          def optionally_accepts_value(_value, options = {})
            _value = _value.nil? ? options[:with] : _value

            context "with a value of #{_value}" do
              accepts_with_200 payload: Proc.new { default_resource_attributes.with(validate_attribute, _value) }
            end

            context "without the attribute" do
              accepts_with_200 payload: Proc.new { default_resource_attributes.except(validate_attribute) } do
                expects(parsed_response[_attribute]).to eql options[:default]
              end
            end
          end

          def forbids_update_to(attribute, options = {})
            context "when attempting to change \"#{attribute}\" attribute", :authorization do
              rejects_with_403 payload: Proc.new { default_resource_attributes.with(attribute, options[:with]) }
            end
          end

        end # ClassMethods
      end # Update
    end # Validations
  end # RSpec
end # Pedant
