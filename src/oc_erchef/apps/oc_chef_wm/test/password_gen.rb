require "digest"
class ShaHasher
  def self.generate_salt
    salt = Time.now.to_s
    chars = ("a".."z").to_a + ("A".."Z").to_a + ("0".."9").to_a
    1.upto(30) { |i| salt << chars[rand(chars.size-1)] }
    salt
  end

  def self.encrypt_password(password, salt, type)
    case type
      when "osc"
        OpenSSL::Digest::SHA1.hexdigest("--#{salt}--#{password}--")
      when "ec"
        OpenSSL::Digest::SHA1.hexdigest("#{salt}--#{password}--")
    end
  end

  def self.make_examples(words)
    ["osc", "ec"].each do |type|
      puts "{ #{type}, ["
      words.each do |w|
        s = self.generate_salt
        puts self.fmt_entry(w, self.encrypt_password(w, s, type), s)
      end
      puts "]}."
    end
  end

  def self.fmt_entry(pass, hash, salt)
    '{<<"%s">>, <<"%s">>, <<"%s">>}' % [pass, hash, salt]
  end
end

words = ["a", "b", "fizzbuz", "sesame", "apple"]
ShaHasher.make_examples(words)
