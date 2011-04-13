require File.join(File.dirname(__FILE__), '..', 'lib', 'version_space')
require File.join(File.dirname(__FILE__), '..', 'lib', 'data_set')

class AQ
  attr_reader :k
  
  def initialize(data_set)
    @data_set = data_set
    @k = Hash.new { |hash, key| hash[key] = [] }
  end
  
  def select_best_generalization(gen_star)
    gen_star.max_by do |g|
      @data_set.positive_examples_for(@current_book).count do |example|
        more_general?(g, example)
      end
    end
  end

  def gen_star(pos_example, negatives)
    vs = VersionSpace.new(pos_example.size)
    vs.positive_example(pos_example)

    negatives.each do |bsp|
      break if vs.terminated?
      vs.negative_example bsp
    end

    return false unless vs.terminated_successfully?

    select_best_generalization(vs.g)
  end

  def gen_concept_space(positives, negatives)
    result = []
    until positives.empty?
      s = gen_star(positives.shift, negatives)
      next unless s

      result << s
      positives.reject! { |bsp| more_general?(s, bsp) }
    end
    result
  end
  
  def generate_concept_spaces
    @data_set.group_names.each do |book|
      @current_book = book
      @k[book] = gen_concept_space(@data_set.positive_examples_for(book), @data_set.negative_examples_for(book))
    end
  end
end

@data_set = DataSet.new(File.join(File.dirname(__FILE__), "input.csv"), :use_columns => 1..-2)
@aq = AQ.new(@data_set)
@aq.generate_concept_spaces

k = @aq.k

#@data_set = DataSet.new(File.join(File.dirname(__FILE__), "test.csv"), :use_columns => 0..-2)

not_detected = 0
@data_set.group_names.each do |book|
  @data_set.positive_examples_for(book).each do |example|
    correct = 0
    wrong = 0

    k.each do |k_book, k_values|
      k_values.each do |k_value|

        if more_general?(k_value, example)
          if k_book == book
            correct += 1
          else
            wrong += 1
          end
        end
      end
    end

    if correct.zero? && wrong.zero?
      puts "---"
      puts "No match for:"
      p example
      not_detected += 1
    elsif !wrong.zero? && !correct.zero?
      puts "---"
      puts "Ambiguous match for:"
      p example
      not_detected += 1
    elsif !wrong.zero?
      not_detected += 1
      puts "---"
      puts "Wrong match for:"
      p example
    end

    # puts "---"
    # puts "Correct: #{correct}"
    # puts "Wrong: #{wrong}"
  end
end

puts "Nicht erkannt: #{not_detected} von: #{@data_set.lines.size}"

# groups.each do |book, examples|
#   examples.each do |example|
#     found = false
# 
#     k.each do |key, value|
#       value.each do |kv|
#         if more_general?(kv, normalize_example(example))
#           found = true
#         end
#       end
#     end
#     
#     if found == false
#       puts "---"
#       puts "didn't find a match! "
#       p example
#       puts "---"
#       puts
#     end
#   end
# end

require "pp"
pp k


