#!/usr/bin/env ruby

require 'bundler/setup'
require 'sequel'
require 'logger'

PGSQL_CMD = "psql -q -d postgres -h localhost -p 5432 -f -"

module SchemaHelper
  DB_NAME = "chef_db_itest"
  DB_USER = "itest"
  DB_PASS = "itest"

  PGSQL_SCHEMA ="../priv/pgsql_schema.sql"
  MYSQL_SCHEMA ="../priv/mysql_schema.sql"

  MYSQL_ROOT_PASS = ""
  PGSQL_ROOT_PASS = ""

  class Pgsql
    attr_reader :root_db, :url, :root_url

    def initialize
      # currently we don't use a password for postgress?
      @url = "postgres://#{DB_USER}@localhost/#{DB_NAME}"
      # Default postgres setup is keyed off user? This is true for dev
      # setup on OS X, not sure it will make any sense elsewhere :(
      @root_url = "postgres://#{ENV['USER']}@localhost/postgres"
      require 'pg'
      @root_db = Sequel.connect(root_url, :loggers => [Logger.new(STDOUT)])
    end

    def create_user
      root_db << "create user #{DB_USER}"
    end

    def create_db
      root_db << "create database #{DB_NAME} owner #{DB_USER}"
      root_db << "grant all privileges on database #{DB_NAME} to #{DB_USER}"
    end

    def drop_db
      root_db << "drop database #{DB_NAME}"
    end

    def drop_user
      root_db << "drop user #{DB_USER}"
    end

    def migrate
      cmd = "psql -U #{DB_USER} #{DB_NAME} < #{PGSQL_SCHEMA}"
      puts "running migration: #{cmd}"
      system(cmd)
    end
  end

  class Mysql
    attr_reader :root_db, :url, :root_url

    def initialize
      @url = "mysql2://#{DB_USER}:#{DB_PASS}@localhost:3306/#{DB_NAME}"
      @root_url = "mysql2://root@localhost:3306/mysql"
      require 'mysql2'
      @root_db = Sequel.connect(root_url, :loggers => [Logger.new(STDOUT)])
    end

    def create_user
      root_db << "create user '#{DB_USER}'@'localhost' identified by '#{DB_PASS}'"
    end

    def create_db
      root_db << "create database #{DB_NAME}"
      root_db << "grant all on #{DB_NAME}.* to '#{DB_USER}'@'localhost'"
    end

    def drop_db
      root_db << "drop database #{DB_NAME}"
    end

    def drop_user
      root_db << "drop user '#{DB_USER}'@'localhost'"
    end

    def migrate
      cmd = "mysql -u root #{DB_NAME} < #{MYSQL_SCHEMA}"
      puts "running migration: #{cmd}"
      system(cmd)
    end
  end
end

helper = case ARGV[0]
when "mysql"
  SchemaHelper::Mysql.new
when "pgsql"
  SchemaHelper::Pgsql.new
end

case ARGV[1]
when "create"
  helper.create_user
  helper.create_db
  helper.migrate
when "destroy"
  helper.drop_db
  helper.drop_user
else
  puts "unknown action, please specify 'create' or 'destroy'"
end
