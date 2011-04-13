class DataSet
  START_AFTER_LINE = 1
  GROUP_BY_COLUMN = -1
  
  def initialize(file, options = {})
    @file = file
    @options = {
      :start_after_line => 1,
      :group_by_column => -1,
      :use_columns => 1..-2
    }.merge(options)
  end

  def lines
    lines = File.readlines(@file)
    @options[:start_after_line].times { lines.shift }
    lines.map { |line| line.chomp.split(';').compact }.reject { |line| line.empty? }
  end

  def grouped_examples
    @groups ||= lines.group_by { |line| line.last }
  end
  
  def group_names
    grouped_examples.keys
  end
  
  def normalize_example(example)
    example[@options[:use_columns]]
  end
  
  def positive_examples_for(key)
    grouped_examples[key].map { |ex| normalize_example(ex) }
  end
  
  def negative_examples_for(key)
    (grouped_examples.keys - [key]).inject([]) do |acc, key|
      acc.concat(grouped_examples[key].map { |ex| normalize_example(ex) })
    end
  end
end