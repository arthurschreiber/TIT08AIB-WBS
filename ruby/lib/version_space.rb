class VersionSpace
  attr_reader :s, :g
  
  def initialize(g, s)
    @g = g
    @s = s
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
  
  def done?
    @g == @s || @g.empty? || @s.empty?
  end
  
  def negative_example(example)
    # Lösche alle aus @s die dem Beispiel entsprechen
    @s.reject! { |s| more_general?(s, example) }
    
    # Lösche alle Elemente aus @g, die das Beispiel enthalten
    # durch die Spezialisierungen, die das Beispiel _nicht_ enthalten,
    # aber alle vorhergegangenen positiven Beispiele
    @g = @g.inject([]) do |acc, g|
      if !more_general?(g, example)
        acc << g
      else
        specialize(g, example, @s.first).each do |new_g|
          next if more_general?(new_g, example)
          
          if @positive_examples.all? { |pe| more_general?(new_g, pe) } && !@g.any? { |other_g| g != other_g && (new_g & other_g) != [:*] && more_general?(other_g, new_g) }
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
  
  def includes?(a, b)
    a == b || a == :*
  end

  def more_general?(a, b)
    a.each_with_index.all? { |item, i| includes?(item, b[i]) }
  end

  def generalize(hyp1, hyp2)
    return [] if hyp1 == []

    hyp1.each_with_index.map do |item, index|
      if item == :_
        hyp2[index]
      elsif hyp2[index] == :_ || item == hyp2[index]
        item
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
        copy = g.dup
        copy[index] = s[index]
        copy
      end
    end
  end
end