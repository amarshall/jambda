require 'simplecov'
SimpleCov.start do
  coverage_dir 'tmp/coverage'
end

require 'jambda/util'

RSpec.configure do |c|
  c.extend Jambda::Util
  c.order = 'random'

  c.after do
    defined?(Jambda::Eval) and Jambda::Eval.reset!
  end
end
