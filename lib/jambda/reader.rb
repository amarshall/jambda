require 'jambda'

module Jambda::Reader; end

class << Jambda::Reader
  def tokenize str
    str.split(' ').flat_map { |s| s.split(/\b/) }
  end
end
