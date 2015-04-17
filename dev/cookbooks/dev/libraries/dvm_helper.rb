require "pathname"

module DVMHelper
  def self.dvm_path
    path = `gem which dvm 2>&1`
    if path =~ /^ERROR.*/
      nil
    else
      # comes back as PATH/dvm/dvm.rb
      Pathname.new(path).parent.parent.to_s
    end
  end
end
