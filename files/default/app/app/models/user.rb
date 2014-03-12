class User 
  extend ActiveModel::Naming
  include ActiveModel::AttributeMethods

  attr_accessor :username, :password, :first_name, :last_name, :email, :public_key

  def initialize(args={})
    args.each do |k,v|
      instance_variable_set("@#{k}", v) unless v.nil?
    end
  end

  def id
    to_param
  end

  def to_param
    username
  end

  def self.find(params)
    find_by_username(params[:uid])
  end

  def self.find_by_username(username)
    user = fake_chef_user(username)
    new(user) if user
  end

  private

    def self.fake_chef_user(username)
      fake_user if username == fake_user[:username]
    end

    def self.fake_user
      { 
        :username => 'applejack',
        :password => 'applejack',
        :first_name => 'Apple',
        :last_name => 'Jack',
        :email => 'applejack@ponyville.com',
        :public_key => %Q{-----BEGIN PUBLIC KEY-----
  MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAv8QZxdp5XQVGnpHgPdRn
  MeHghbzDW/D6oAcXT6spfqN+5T7W/TruYpJbL+9cDfrIoNW8YOvHhDp0yoHl/YNl
  ZX0bYltdgZer10/Yv9PoB2U4TAzajBcd3DF3TxiB1sBPxqLvcF30CkmPpq4lmsNs
  n/L6OlcrGk26TMEhZwxw9tx8sl50DVlWm9GfefvVeZHfk1d1c5Yi/YfMiX688zRI
  SzQ2i3KSq450nfaX0p4dnRq5cq7/qW+Yr11lRByTIq6j8qEwPJNIXsUwIDWab8fr
  F6dutenFO3xjG+s12x8iU8MQLzsBMtFa9V1hr189xqUwAW0DBoiKBXjkQ20DKC/T
  SQIDAQAB
  -----END PUBLIC KEY-----}
      }
    end

end