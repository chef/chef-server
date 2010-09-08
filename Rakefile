require 'spec/rake/spectask'

Spec::Rake::SpecTask.new(:spec) do |spec|
  spec.libs << 'lib' << 'spec'
  spec.spec_opts = ['--options', "\"#{File.dirname(__FILE__)}/spec/spec.opts\""]
  spec.spec_files = FileList['spec/**/*_spec.rb']
end

desc "install gem dependencies"
task :bundle do
  system("bundle install")
end

task :default => :spec
