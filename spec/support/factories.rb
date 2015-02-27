require 'factory_girl'

FactoryGirl.define do
  factory :user do
    username 'applejack'
    password 'password'
    first_name 'Apple'
    last_name 'Jack'
    email 'applejack@ponyville.com'
    public_key %Q{-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAv8QZxdp5XQVGnpHgPdRn
MeHghbzDW/D6oAcXT6spfqN+5T7W/TruYpJbL+9cDfrIoNW8YOvHhDp0yoHl/YNl
ZX0bYltdgZer10/Yv9PoB2U4TAzajBcd3DF3TxiB1sBPxqLvcF30CkmPpq4lmsNs
n/L6OlcrGk26TMEhZwxw9tx8sl50DVlWm9GfefvVeZHfk1d1c5Yi/YfMiX688zRI
SzQ2i3KSq450nfaX0p4dnRq5cq7/qW+Yr11lRByTIq6j8qEwPJNIXsUwIDWab8fr
F6dutenFO3xjG+s12x8iU8MQLzsBMtFa9V1hr189xqUwAW0DBoiKBXjkQ20DKC/T
SQIDAQAB
-----END PUBLIC KEY-----}
  end

  factory :administrator, parent: :user do
    username 'rainbowdash'
    password 'password'
    first_name 'Rainbow'
    last_name 'Dash'
    email 'rainbowdash@ponyville.com'
    public_key %Q{-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAv8QZxdp5XQVGnpHgPdRn
MeHghbzDW/D6oAcXT6spfqN+5T7W/TruYpJbL+9cDfrIoNW8YOvHhDp0yoHl/YNl
ZX0bYltdgZer10/Yv9PoB2U4TAzajBcd3DF3TxiB1sBPxqLvcF30CkmPpq4lmsNs
n/L6OlcrGk26TMEhZwxw9tx8sl50DVlWm9GfefvVeZHfk1d1c5Yi/YfMiX688zRI
SzQ2i3KSq450nfaX0p4dnRq5cq7/qW+Yr11lRByTIq6j8qEwPJNIXsUwIDWab8fr
F6dutenFO3xjG+s12x8iU8MQLzsBMtFa9V1hr189xqUwAW0DBoiKBXjkQ20DKC/T
SQIDAQAB
-----END PUBLIC KEY-----}
  end
end
