#!/bin/bash

set -o errexit -o nounset -o verbose

#
# setting up build environment
#

# os package gardening:
yum -y remove ruby-libs ruby-rdoc ruby-ri ruby-irb rubygems ruby ruby-devel ruby-libs ruby-devel ruby-docs
yum -y install gcc bison flex make gcc-c++ zlib-devel readline-devel openssl-devel

# download our sources:
cd /tmp
wget http://ftp.ruby-lang.org/pub/ruby/1.9/ruby-1.9.2-p180.tar.gz
wget http://production.cf.rubygems.org/rubygems/rubygems-1.8.12.tgz

# install a working ruby into /usr/local/bin
export PATH=/usr/local/bin:$PATH
rm -rf ruby-1.9.2-p180 
tar zxf ruby-1.9.2-p180.tar.gz
cd ruby-1.9.2-p180
./configure --prefix=/usr/local
make
make install 

# install a working rubygems into /usr/local/bin
cd /tmp
rm -rf rubygems-1.8.12
tar zxf rubygems-1.8.12.tgz
cd rubygems-1.8.12
/usr/local/bin/ruby setup.rb

# install fpm into /usr/local (0.4.3 is broken, we need 0.3.11)
/usr/local/bin/gem install fpm -v 0.3.11

#
# building shortbus-ruby
#

# install our /usr/bin/ruby into /tmp/ruby for packaging
cd /tmp
rm -rf ruby-1.9.2-p180 ruby
tar zxf ruby-1.9.2-p180.tar.gz
cd ruby-1.9.2-p180
./configure --prefix=/usr
make
mkdir /tmp/ruby
make install DESTDIR=/tmp/ruby

# package up our ruby-shortbus
cd /tmp
/usr/local/bin/fpm -s dir -t rpm -n shortbus-ruby -v 1.9.2p180 --iteration 1 -C /tmp/ruby --conflicts ruby,ruby-libs,ruby-irb,ruby-rdoc,ruby-ri,ruby-devel,ruby-docs usr

#
# building shortbus-rubygems
#

# install our rubygems into /tmp/rubygems for packaging
cd /tmp
rm -rf rubygems-1.8.12 rubygems
tar zxf rubygems-1.8.12.tgz
cd rubygems-1.8.12

# annoying magic foo follows (ripped off from rubygems SRPM spec file in Centos 6)
buildroot=/tmp/rubygems
gem_dir=/usr/lib/ruby/gems
rb_ver=1.9.1
gem_home=${gem_dir}/${rb_ver}
ruby_sitelib=/usr/lib/ruby/site_ruby/1.9.1
 
cd /tmp/rubygems-1.8.12
 
rm -rf ${buildroot}
mkdir ${buildroot}
 
GEM_HOME=${buildroot}/${gem_home} \
    ruby setup.rb --prefix=/ \
        --no-rdoc --no-ri \
        --destdir=${buildroot}/${ruby_sitelib}/
 
mkdir ${buildroot}/usr/bin
mv ${buildroot}/${ruby_sitelib}/bin/gem ${buildroot}/usr/bin/gem
rm -rf ${buildroot}/${ruby_sitelib}/bin
mv ${buildroot}/${ruby_sitelib}/lib/* ${buildroot}/${ruby_sitelib}/.
mkdir -p ${buildroot}/%{gem_home}/{cache,gems,specifications,doc}

# thank god that's over, now package up the shortbus-rubygems
cd /tmp
fpm -s dir -t rpm -n shortbus-rubygems -v 1.8.12 --iteration 1 -C /tmp/rubygems -d shortbus-ruby --conflicts rubygems usr

#
# building shortbus-gems
#

# gems installed into our /usr/local gem paths will cause us issues now
/usr/local/bin/gem list | cut -f1 -d ' ' | xargs gem uninstall

# ugly magic to install gems into /tmp/gems
buildroot=/tmp/gems
gem_dir=/usr/lib/ruby/gems
rb_ver=1.9.1
gem_home=${gem_dir}/${rb_ver}
ruby_sitelib=/usr/lib/ruby/site_ruby/1.9.1
 
rm -rf ${buildroot}
mkdir -p ${buildroot}
mkdir -p ${buildroot}/usr/bin
 
GEM_HOME=${buildroot}/${gem_home} gem install ohai chef bundler --no-ri --no-rdoc --bindir=${buildroot}/usr/bin/ -i /tmp/gems/usr/lib/ruby/gems/1.9.1
GEM_HOME=${buildroot}/${gem_home} gem install fpm -v 0.3.11 --no-ri --no-rdoc --bindir=${buildroot}/usr/bin/ -i /tmp/gems/usr/lib/ruby/gems/1.9.1

cd /tmp
# use the fpm we just installed to package itself up
GEM_HOME=${buildroot}/${gem_home} /tmp/gems/usr/bin/fpm -s dir -t rpm -n shortbus-gems -v 1.8.12 --iteration 1 -C /tmp/gems -d shortbus-ruby --conflicts rubygems usr

# dump everything back into /vagrant
cp -f /tmp/shortbus* /vagrant

