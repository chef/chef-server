require 'mixlib/shellout'

class OmnibusHelper
  attr_reader :node

  def initialize(node)
    @node = node
  end

  def ownership
    owner = node['private_chef']['user']['username']
    group = owner

    {"owner" => owner, "group" => group}
  end

  # Normalizes hosts. If the host part is an ipv6 literal, then it
  # needs to be quoted with []
  def self.normalize_host(host_part)
    # Make this simple: if ':' is detected at all, it is assumed
    # to be a valid ipv6 address. We don't do data validation at this
    # point, and ':' is only valid in an URL if it is quoted by brackets.
    if host_part =~ /:/
      "[#{host_part}]"
    else
      host_part
    end
  end

  def normalize_host(host_part)
    self.class.normalize_host(host_part)
  end

  def vip_for_uri(service)
    normalize_host(node['private_chef'][service]['vip'])
  end

  def db_connection_uri
    db_protocol = "postgres"
    db_user     = node['private_chef']['postgresql']['sql_user']
    db_password = node['private_chef']['postgresql']['sql_password']
    db_vip      = vip_for_uri('postgresql')
    db_name     = "opscode_chef"

    "#{db_protocol}://#{db_user}:#{db_password}@#{db_vip}/#{db_name}"
  end

  def bifrost_db_connection_uri
    db_protocol = "postgres"
    db_user     = node['private_chef']['oc_bifrost']['sql_user']
    db_password = node['private_chef']['oc_bifrost']['sql_password']
    db_vip      = vip_for_uri('postgresql')
    db_name     = "bifrost"

    "#{db_protocol}://#{db_user}:#{db_password}@#{db_vip}/#{db_name}"
  end

  # This file is touched once initial bootstrapping of the system is
  # done.
  def self.bootstrap_sentinel_file
    "/var/opt/opscode/bootstrapped"
  end

  # Use the presence of a sentinel file as an indicator for whether
  # the server has already had initial bootstrapping performed.
  #
  # @todo: Is there a more robust way to determine this, i.e., based
  #   on some functional aspect of the system?
  def self.has_been_bootstrapped?
    File.exists?(bootstrap_sentinel_file)
  end

  # Parse a config string as a memory value returning an integer in MB
  # units.  Supported inputs (not case sensitive) are B, K/Kb, M/Mb,
  # G/Gb. Uses integer division so values in B and Kb must exceed 1Mb.
  def self.parse_mem_to_mb(mem_str)
    if mem_str.is_a?(Integer)
      return mem_str
    end
    regex = /(\d+)([GgmMkKbB]{0,2})/
    m  = regex.match(mem_str)
    raise "bad arg" if !m
    raw_value = m[1].to_i
    units = m[2]
    value = case units
            when /^b$/i
              raw_value / (1024 * 1024)
            when /^kb?$/i
              raw_value / 1024
            when /^mb?$/i
              raw_value
            when ""                       # no units, assume Mb
              raw_value
            when /^gb?$/i
              raw_value * 1024
            else
              raise "unsupported memory units: #{mem_str}"
            end
    if value > 0
      value
    else
      raise "zero Mb value not allowed: #{mem_str}"
    end
  end

  # generate a certificate signed by the opscode ca key
  #
  # === Returns
  # [cert, key]
  #
  def self.gen_certificate
    key = OpenSSL::PKey::RSA.generate(2048)
    public_key = key.public_key
    cert_uuid = UUIDTools::UUID.random_create
    common_name = "URI:http://opscode.com/GUIDS/#{cert_uuid}"
    info = [["C", "US"], ["ST", "Washington"], ["L", "Seattle"], ["O", "Opscode, Inc."], ["OU", "Certificate Service"], ["CN", common_name]]
    cert = OpenSSL::X509::Certificate.new
    cert.subject = OpenSSL::X509::Name.new(info)
    cert.issuer = ca_certificate.subject
    cert.not_before = Time.now
    cert.not_after = Time.now + 10 * 365 * 24 * 60 * 60 # 10 years
    cert.public_key = public_key
    cert.serial = 1
    cert.version = 3

    ef = OpenSSL::X509::ExtensionFactory.new
    ef.subject_certificate = cert
    ef.issuer_certificate = ca_certificate
    cert.extensions = [
                       ef.create_extension("basicConstraints","CA:FALSE",true),
                       ef.create_extension("subjectKeyIdentifier", "hash")
                      ]
    cert.sign(ca_keypair, OpenSSL::Digest::SHA1.new)

    return cert, key
  end

  ######################################################################
  #
  # the following is the Opscode CA key and certificate, copied from
  # the cert project(s)
  #
  ######################################################################

  def self.ca_certificate
    @_ca_cert ||=
      begin
        cert_string = <<-EOCERT
-----BEGIN CERTIFICATE-----
MIIDyDCCAzGgAwIBAwIBATANBgkqhkiG9w0BAQUFADCBnjELMAkGA1UEBhMCVVMx
EzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcMB1NlYXR0bGUxFjAUBgNVBAoM
DU9wc2NvZGUsIEluYy4xHDAaBgNVBAsME0NlcnRpZmljYXRlIFNlcnZpY2UxMjAw
BgNVBAMMKW9wc2NvZGUuY29tL2VtYWlsQWRkcmVzcz1hdXRoQG9wc2NvZGUuY29t
MB4XDTA5MDUwNjIzMDEzNVoXDTE5MDUwNDIzMDEzNVowgZ4xCzAJBgNVBAYTAlVT
MRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRYwFAYDVQQK
DA1PcHNjb2RlLCBJbmMuMRwwGgYDVQQLDBNDZXJ0aWZpY2F0ZSBTZXJ2aWNlMTIw
MAYDVQQDDClvcHNjb2RlLmNvbS9lbWFpbEFkZHJlc3M9YXV0aEBvcHNjb2RlLmNv
bTCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAlKTCZPmifZe9ruxlQpWRj+yx
Mxt6+omH44jSfj4Obrnmm5eqVhRwjSfHOq383IeilFrNqC5VkiZrlLh8uhuTeaCy
PE1eED7DZOmwuswTui49DqXiVE39jB6TnzZ3mr6HOPHXtPhSzdtILo18RMmgyfm/
csrwct1B3GuQ9LSVMXkCAwEAAaOCARIwggEOMA8GA1UdEwEB/wQFMAMBAf8wHQYD
VR0OBBYEFJ228MdlU86GfVLsQx8rleAeM+eLMA4GA1UdDwEB/wQEAwIBBjCBywYD
VR0jBIHDMIHAgBSdtvDHZVPOhn1S7EMfK5XgHjPni6GBpKSBoTCBnjELMAkGA1UE
BhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcMB1NlYXR0bGUxFjAU
BgNVBAoMDU9wc2NvZGUsIEluYy4xHDAaBgNVBAsME0NlcnRpZmljYXRlIFNlcnZp
Y2UxMjAwBgNVBAMMKW9wc2NvZGUuY29tL2VtYWlsQWRkcmVzcz1hdXRoQG9wc2Nv
ZGUuY29tggEBMA0GCSqGSIb3DQEBBQUAA4GBAHJxAnwTt/liAMfZf5Khg7Mck4f+
IkO3rjoI23XNbVHlctTOieSwzRZtBRdNOTzQvzzhh1KKpl3Rt04rrRPQvDeO/Usm
pVr6g+lk2hhDgKKeR4J7qXZmlemZTrFZoobdoijDaOT5NuqkGt5ANdTqzRwbC9zQ
t6vXSWGCFoo4AEic
-----END CERTIFICATE-----
EOCERT
        OpenSSL::X509::Certificate.new(cert_string)
      end
  end

  def self.ca_keypair
    @_ca_key ||=
      begin
        keypair_string = <<-EOKEY
-----BEGIN RSA PRIVATE KEY-----
MIICWwIBAAKBgQCUpMJk+aJ9l72u7GVClZGP7LEzG3r6iYfjiNJ+Pg5uueabl6pW
FHCNJ8c6rfzch6KUWs2oLlWSJmuUuHy6G5N5oLI8TV4QPsNk6bC6zBO6Lj0OpeJU
Tf2MHpOfNneavoc48de0+FLN20gujXxEyaDJ+b9yyvBy3UHca5D0tJUxeQIDAQAB
AoGAYAPRIeJyiIfk2cIPYqQ0g3BTwfyFQqJl6Z7uwOca8YEZqfWc7L+FOFiyg3/x
rw3aAdRptbJASgiRQ16sCpdXeaRFY5gcO2MnqmCyoyp2//zhdFReSC+Akim1UPtG
5SqqdV9I0TBl+1JlMiivn677mXGij+qyQjSWxW2pGVsbTSUCQQDDLb/DgoD0+N6O
FIoJ/Mh5cgIxQhqXu/dylEv/I3goSJdXPAqhsnsa6zYQGdftnvMK1ZXS/hYL4i06
w9lKDV8PAkEAwvaz1oUtXLNfYYAF42c1BoBhqCzjXSzMWPu5BlWQzSsdzgVgDuX3
LlkiIdRtMcMaNskaBTtIClCxaEm3rUnm9wJAEOp2JEu7QYAQSeAd1p/CAESRTBOe
mmgAGj4gGAzK7TLdawIZKcp+QOcB2INk44NTLS01vwOmhYEkymMPAgwGoQJAKimq
GMFyXvLXtME4BMbEG+TVucYDYZoXk0LU776/cu9ZIb3d2Tr4asiR7hj/iFx2JdT1
0J3SZZCv3SrcExjBXwJABS3/iQroe24tvrmyy4tc5YG5ygIRaBUCs6dn0fbisX/9
K1oq5Lnwimy4l2NI0o/lxIqnwFilACjs3tuXH1OhMA==
-----END RSA PRIVATE KEY-----
EOKEY
        OpenSSL::PKey::RSA.new(keypair_string)
      end
  end

  def self.erl_atom_or_string(term)
    case term
    when Symbol
      term
    when String
      "\"#{term}\""
    else
      "undefined"
    end
  end

  def erl_atom_or_string(term)
    self.class.erl_atom_or_string(term)
  end

  def s3_url_caching(setting)
    case setting.to_s
    when "off"
      "off"
    when /m$/
      "{#{setting.chop}, minutes}"
    when /%$/
      "{#{setting.chop}, percent}"
    end
  end

  # OC-11540, fallback to ssl_port if non_ssl_port is disabled
  def internal_lb_url
    if node['private_chef']['nginx']['non_ssl_port'] == false
      "https://#{vip_for_uri('lb_internal')}:#{node['private_chef']['nginx']['ssl_port']}"
    else
      "http://#{vip_for_uri('lb_internal')}:#{node['private_chef']['nginx']['non_ssl_port']}"
    end
  end

  def ldap_authentication_enabled?
    node['private_chef'].attribute?('ldap') &&
      !(node['private_chef']['ldap'].nil? || node['private_chef']['ldap'].empty?)
  end
end

