module Jambda::BacktraceFilter; end

class << Jambda::BacktraceFilter
  def filter(backtrace)
    top, *rest = backtrace
    rest = rest.select do |line|
      line =~ /\/(special_forms|core)\.rb\b/
    end
    [top, *rest]
  end

  def call(*args)
    filter(*args)
  end
end
