require File.join(File.dirname(__FILE__), '..', 'lib', 'version_space')

lines = File.readlines(File.join(File.dirname(__FILE__), "input.csv"))
lines.shift

lines.map! do |line|
  line = line.chomp.split(';')
  line.shift
  line
end

k = Hash.new { |hash, key| hash[key] = [] }

groups = lines.group_by { |line| line.last }
groups.each do |book, examples|
  positive_beispiele = examples.dup
  negative_beispiele = (groups.keys - [book]).inject([]) { |acc, key| acc.concat(groups[key]) }

  until positive_beispiele.empty?
    a = positive_beispiele.shift

    vs = VersionSpace.new([[:*] * (a.size - 1)], [a[0..-2]])
    negative_beispiele.each do |bsp|
      break if vs.done?
      vs.negative_example bsp[0..-2]
    end
  
    s = vs.g.first
    next unless s
    k[book] << s
    positive_beispiele.reject! { |bsp| vs.more_general?(s, bsp) }
  end
end

require "pp"

pp k


