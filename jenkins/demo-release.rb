#! /usr/bin/env ruby

release_version = ENV['RELEASE_VERSION']

platforms = {
  "ubuntu-10.04" => lambda {|ver| "private-chef_#{ver}-1.ubuntu.10.04_amd64.deb"},
  "ubuntu-11.04" => lambda {|ver| "private-chef_#{ver}-1.ubuntu.11.04_amd64.deb"},
  "centos5"      => lambda {|ver| "private-chef-#{ver}-1.el5.x86_64.rpm"},
  "centos6"      => lambda {|ver| "private-chef-#{ver}-1.el6.x86_64.rpm"}
}

jenkins_home = "/var/lib/jenkins"
jenkins_jobs_dir = File.join(jenkins_home, "/jenkins-data/jobs")

package_files = platforms.inject({}) do |res, (platform, package_lambda)|
  package_name = package_lambda.call(release_version)
  job_name = "opscode-omnibus-#{platform}-release"
  job_dir  = File.join(jenkins_jobs_dir, job_name)
  package_file = Dir.glob(File.join(job_dir, "**", package_name)).first
  res[platform] = package_file
  res
end

if package_files.has_value?(nil)
  puts '*' * 70
  puts "Could not find release artifact for version #{release_version} on all platforms!"
  puts '*' * 70
  puts
  package_files.each do |platform, package_file|
    puts "#{platform}: #{package_file}"
  end
  exit 1
else
  puts '*' * 70
  puts "Found release artifacts for version #{release_version} on all platforms. Uploading to S3!"
  puts '*' * 70
  puts
  package_files.each do |platform, package_file|
    artifact_filename = File.basename(package_file)
    demo_filename = platforms[platform].call("demo")
    puts "\t uploading #{artifact_filename} as #{demo_filename}..."
    system("s3cmd put #{package_file} s3://opscode-private-chef-demo/#{demo_filename}")
    status = $?
    if status != 0
      puts "FAILED TO UPLOAD #{artifact_filename}"
      exit 1
    end
  end
end

