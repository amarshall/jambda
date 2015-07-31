require 'jambda'
require 'coderay'

module Jambda::Trace; end

class << Jambda::Trace
  def call
    @trace ||= TracePoint.new(:call) do |point|
      context = point.defined_class.to_s
      if context =~ /Jambda/ && context !~ /Util/
        nesting = point.binding.send(:caller)
          .grep(/lib\/jambda/)
          .reject { |s| s =~ /util/ }
          .size
        lvars = point.binding.local_variables.each_with_object({}) do |lvar, h|
          val = point.binding.local_variable_get(lvar)
          next unless val
          h[lvar] = val
        end
        leader = nesting.times.map { |n| n.even? ? '│ ' : '╎ ' }.join
        $stderr.puts "#{leader}#{point.method_id}: #{CodeRay.scan(lvars.inspect, :ruby).term}"
      end
    end

    @trace.enable
    result = yield
    @trace.disable
    result
  end
end

Proc.send(:prepend, Module.new {
  def inspect
    file, lineno = source_location
    line = File.readlines(file).to_a[lineno - 1].strip
    "<#{self.lambda? ? 'lambda' : 'proc'}('#{line}')>"
  end
})
