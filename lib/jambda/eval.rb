require 'jambda'
require 'jambda/core'
require 'jambda/util'

module Jambda::Eval; end

class << Jambda::Eval
  include Jambda::Util

  def eval_form form
    case form
    when Enumerable
      sym, *args = form
      args = args.map { |arg| eval_form(arg) }
      eval_sym(kernel, sym, *args)
    else form
    end
  end

  def eval_sym env, sym, *args
    env.public_send(sym, *args)
  end

  def kernel
    Module.new do
      extend Jambda::Core
    end
  end
end
