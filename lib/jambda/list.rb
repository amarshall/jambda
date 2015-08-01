require 'jambda'
require 'jambda/util'

class Jambda::List
  include Enumerable

  def initialize(elems)
    @elems = elems
    Jambda::Util.freeze2(self)
  end

  def each(&block)
    return to_enum unless block_given?
    @elems.each(&block)
  end

  def empty?
    @elems.empty?
  end

  def [](index)
    wrap(@elems[index])
  end

  Enumerable.instance_methods.each do |method|
    define_method(method) do |*args, &block|
      wrap(super(*args, &block))
    end
  end

  def inspect
    "<list: #{@elems.inspect}>"
  end

  private

  def wrap(obj)
    obj.is_a?(Array) ? Jambda::List.new(obj) : obj
  end
end
