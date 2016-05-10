module Veil
  class InvalidSalt < StandardError; end
  class InvalidSecret < StandardError; end
  class InvalidParameter < StandardError; end
  class InvalidHasher < StandardError; end
  class InvalidCredentialCollectionFile < StandardError; end
  class MissingParameter < StandardError; end
  class NotImplmented < StandardError; end
  class InvalidCredentialHash < StandardError; end
end
