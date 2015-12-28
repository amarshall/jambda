module Jambda
  class Error < StandardError
    attr_accessor :jambda_trace

    def initialize(message, jambda_trace)
      super(message)
      @jambda_trace = jambda_trace
    end
  end
end
