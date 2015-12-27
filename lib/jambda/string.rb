require 'jambda'
require 'jambda/util'

class Jambda::String
  def initialize(str)
    @str = str.to_str
    Jambda::Util.freeze2(self)
  end

  def +(other)
    self.class.new(@str + other.to_s)
  end

  def ==(other)
    @str == other.to_str
  end

  def to_s
    @str
  end

  def inspect
    @str.inspect
  end
end
