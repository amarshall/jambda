require 'jambda'
require 'jambda/core'
require 'jambda/util'

module Jambda::Eval; end

class << Jambda::Eval
  include Jambda::Util

  def eval_form form
    case form
    when Enumerable
      eval_sym(kernel, form.first, *rest(form))
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
