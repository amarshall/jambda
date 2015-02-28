require 'jambda'
require 'jambda/util'

module Jambda::Reader; end

class << Jambda::Reader
  include Jambda::Util

  def read_str str
    form, _ = read_form([].freeze, tokenize(str))
    form
  end

  def read_form form, tokens
    nform, ntokens = case peek(tokens)
                     when '(' then read_list(rest(tokens))
                     when ')' then return freeze2([form, rest(tokens)])
                     when nil then return freeze2([form, []])
                     else read_atom(tokens)
                     end
    read_form(freeze2(form + [nform]), ntokens)
  end

  def read_atom tokens # -> [form, tokens]
    atom = peek(tokens)
    atom = case atom
           when /\A\d+\z/ then Integer(atom)
           else atom
           end
    [atom, rest(tokens)]
  end

  def read_list tokens
    read_form([].freeze, tokens)
  end

  def tokenize str
    tokens = str.gsub(/\s*;.*\z/, '')
      .split(/(\b|(?<=[()]))/)
      .map(&:strip).reject(&:empty?)
    freeze2(tokens)
  end

  private def log
    require 'coderay'
    @trace ||= TracePoint.new(:call) do |point|
      if point.defined_class == Jambda::Reader.singleton_class
        nesting = point.binding.send(:caller).grep(/lib\/jambda/).size - 1
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
