require File.join(File.dirname(__FILE__), "utils")

class VersionSpace
  attr_reader :s, :g
  
  def initialize(size)
    @g = [[:*] * size]
    @s = [[:_] * size]
    @positive_examples = []
  end
  
  def positive_example(example)
    ###
    # Positives Beispiel
    @positive_examples << example
    
    # Lösche alle aus @g die nicht dem Beispiel entsprechen
    @g.reject! { |g| !more_general?(g, example) }
    
    # Ersetze alle aus @s, die nicht das Beispiel enthalten,
    # durch die speziellste verallgemeinerung
    @s.map! do |s|
      if !more_general?(s, example)
        generalize(s, example)
      else
        s
      end
    end
    
    # Streiche alle Elemente aus @s, die allgemeiner sind als
    # ein Element aus @g
    @s.reject! do |s|
      @g.any? do |g|
        more_general?(s, g)
      end
    end
  end
  
  def terminated?
    @g == @s || @g.empty? || @s.empty?
  end
  
  def terminated_successfully?
    not @g.empty? and not @s.empty? 
  end
  
  def negative_example(example)
    # Lösche alle aus @s die dem Beispiel entsprechen
    @s.reject! { |s| more_general?(s, example) }
    
    return if @s.empty?
    
    # Lösche alle Elemente aus @g, die das Beispiel enthalten
    # durch die Spezialisierungen, die das Beispiel _nicht_ enthalten,
    # aber alle vorhergegangenen positiven Beispiele
    @g = @g.inject([]) do |acc, g|
      if !more_general?(g, example)
        acc << g
      else
        specialize(g, example, @s.first).each do |new_g|
          next if more_general?(new_g, example)
          
          if @positive_examples.all? { |pe| more_general?(new_g, pe) } && !@g.any? { |other_g| g != other_g && more_general?(other_g, new_g) }
            acc << new_g
          end
        end
      end

      acc
    end
    
    # Streiche alle Elemente aus @g, die spezieller sind als ein Element
    # aus @s
    @g.reject! { |g| @s.any? { |s| more_general?(s, g) }}
  end

  def generalize(hyp1, hyp2)
    hyp1.zip(hyp2).map do |a, b|
      if a == :_
        b
      elsif a == b || b == :_
        a
      else
        :*
      end
    end
  end
  
  def get_potential_positions(g, neg, s)
    g.each_index.find_all do |index|
      g[index] == :* && s[index] != neg[index]
    end
  end

  def specialize(g, neg, s)
    get_potential_positions(g, neg, s).map do |index|
      if g[index] == :*
        g.dup.tap { |copy| copy[index] = s[index] }
      end
    end
  end
end