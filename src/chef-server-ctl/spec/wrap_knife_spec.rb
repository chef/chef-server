require 'spec_helper'
require 'ostruct'

# Load the wrap-knife plugin functions
load File.expand_path('../../plugins/wrap-knife.rb', __FILE__)

describe 'wrap-knife plugin' do
  describe '#transform_knife_opc_args' do
    context 'user-list command' do
      it 'transforms --all-info flag to --verbose' do
        args = ['--all-info', 'otherarg']
        result = transform_knife_opc_args(args, 'user-list', 'user', 'list')
        expect(result).to eq(['otherarg', '--verbose'])
      end

      it 'transforms -a flag to --verbose' do
        args = ['-a']
        result = transform_knife_opc_args(args, 'user-list', 'user', 'list')
        expect(result).to eq(['--verbose'])
      end

      it 'transforms -a flag with other arguments to --verbose' do
        args = ['-a', '--with-uri']
        result = transform_knife_opc_args(args, 'user-list', 'user', 'list')
        expect(result).to eq(['--with-uri', '--verbose'])
      end

      it 'handles user-list without -a or --all-info flags' do
        args = ['--with-uri']
        result = transform_knife_opc_args(args, 'user-list', 'user', 'list')
        expect(result).to eq(['--with-uri'])
      end

      it 'handles empty arguments' do
        args = []
        result = transform_knife_opc_args(args, 'user-list', 'user', 'list')
        expect(result).to eq([])
      end
    end

    context 'user-create command' do
      it 'transforms knife-opc format to native knife format with 5 positional args' do
        args = ['user4', 'user', 'four', 'kallol.roy4@progress.com', 'pass1234']
        result = transform_knife_opc_args(args, 'user-create', 'user', 'create')
        expected = ['user4', '--email', 'kallol.roy4@progress.com', '--password', 'pass1234', '--first-name', 'user', '--last-name', 'four']
        expect(result).to eq(expected)
      end

      it 'transforms knife-opc format with middle name (6 positional args)' do
        args = ['user5', 'first', 'middle', 'last', 'test@example.com', 'password123']
        result = transform_knife_opc_args(args, 'user-create', 'user', 'create')
        expected = ['user5', '--email', 'test@example.com', '--password', 'password123', '--first-name', 'first', '--last-name', 'last']
        expect(result).to eq(expected)
      end

      it 'transforms knife-opc format with --filename flag' do
        args = ['testuser', 'Test', 'User', 'test@example.com', 'password', '--filename', '/tmp/key.pem']
        result = transform_knife_opc_args(args, 'user-create', 'user', 'create')
        expected = ['testuser', '--email', 'test@example.com', '--password', 'password', '--first-name', 'Test', '--last-name', 'User', '-f', '/tmp/key.pem']
        expect(result).to eq(expected)
      end
    end

    context 'transform_flags_only' do
      it 'converts --filename to -f' do
        args = ['--filename', '/tmp/test.pem', 'other', 'args']
        result = transform_flags_only(args)
        expect(result).to eq(['-f', '/tmp/test.pem', 'other', 'args'])
      end

      it 'passes through other flags unchanged' do
        args = ['--orgname', 'myorg', '--prevent-keygen']
        result = transform_flags_only(args)
        expect(result).to eq(['--orgname', 'myorg', '--prevent-keygen'])
      end
    end
  end
end
