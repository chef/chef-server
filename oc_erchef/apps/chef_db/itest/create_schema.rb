#!/usr/bin/env ruby

require 'bundler/setup'
require 'logger'

module SchemaHelper
  DB_NAME = "chef_db_itest"
  DB_USER = "itest"
  DB_PASS = "itest"

  PGSQL_SCHEMA ="../priv/pgsql_schema.sql"
  MYSQL_SCHEMA ="../priv/mysql_schema.sql"

  class Pgsql
    def create_user
      system("createuser -DRS #{DB_USER}")
    end

    def create_db
      system("createdb -O #{DB_USER} #{DB_NAME}")
    end

    def drop_db
      system("dropdb #{DB_NAME}")
    end

    def drop_user
      system("dropuser #{DB_USER}")
    end

    def migrate
      cmd = "psql -U #{DB_USER} #{DB_NAME} < #{PGSQL_SCHEMA}"
      puts "running migration: #{cmd}"
      system(cmd)
    end
  end

  class Mysql
    attr_reader :root_db, :db

    def initialize
      @db = SqlCmd.new("mysql --user=#{DB_USER} --password=#{DB_PASS} #{DB_NAME}")
      @root_db = SqlCmd.new("mysql -u root")
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

  class SqlCmd
    def initialize(prefix)
      @cmd_prefix = prefix
    end

    def execute(cmd)
      system("echo \"#{cmd}\"|#{@cmd_prefix}")
    end

    def <<(cmd)
      execute(cmd)
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
