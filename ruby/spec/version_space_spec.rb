# encoding: UTF-8
require File.expand_path('../../lib/version_space', __FILE__)

describe VersionSpace do
  before :each do
    @vs = VersionSpace.new([[:*, :*, :*]], [[:_, :_, :_]])
  end
  
  it "should be correctly building a version space for the 'Sänger/Sängerin' example" do
    @vs.positive_example ["Sängerin", "Jazz", "20er-50er"]
    @vs.g.should == [[:*, :*, :*]]
    @vs.s.should == [["Sängerin", "Jazz", "20er-50er"]]

    @vs.negative_example ["Gruppe", "Pop", "70er"]
    @vs.s.should == [["Sängerin", "Jazz", "20er-50er"]]
    @vs.g.should == [["Sängerin", :*, :*], [:*, "Jazz", :*], [:*, :*, "20er-50er"]]

    @vs.negative_example ["Gruppe", "Pop", "80er"]
    @vs.s.should == [["Sängerin", "Jazz", "20er-50er"]]
    @vs.g.should == [["Sängerin", :*, :*], [:*, "Jazz", :*], [:*, :*, "20er-50er"]]

    @vs.negative_example ["Sänger", "Jazz", "20er-50er"]
    @vs.s.should == [["Sängerin", "Jazz", "20er-50er"]]
    @vs.g.should == [["Sängerin", :*, :*]]

    @vs.positive_example ["Sängerin", "Jazz", "50er-60er"]
    @vs.g.should == [["Sängerin", :*, :*]]
    @vs.s.should == [["Sängerin", "Jazz", :*]]

    @vs.negative_example ["Orchester", "Klassik", "vor 1920"]
    @vs.s.should == [["Sängerin", "Jazz", :*]]
    @vs.g.should == [["Sängerin", :*, :*]]

    @vs.positive_example ["Sängerin", "Jazz", "70er"]
    @vs.g.should == [["Sängerin", :*, :*]]
    @vs.s.should == [["Sängerin", "Jazz", :*]]
  end
  
  it "should be correctly building a version space for the 'Autokauf'" do
    @vs = VersionSpace.new(
      [[:*, :*, :*, :*, :*, :*, :*, :*, :*, :*, :*]],
      [[:_, :_, :_, :_, :_, :_, :_, :_, :_, :_, :_]]
    )
    
    @vs.positive_example(["neu", "VW", "90-120", "< 2 l", "< 180", "Diesel", "< 6 l", "Minivan", "8", "silber/grau", "< 25000"])
    @vs.g.should == [[:*, :*, :*, :*, :*, :*, :*, :*, :*, :*, :*]]
    @vs.s.should == [["neu", "VW", "90-120", "< 2 l", "< 180", "Diesel", "< 6 l", "Minivan", "8", "silber/grau", "< 25000"]]

    @vs.positive_example(["< 2 Jahre", "VW", "90-120", "< 2 l", "< 180", "Diesel", "< 6 l", "Minivan", "8", "grün", "< 20000"])
    @vs.g.should == [[:*, :*, :*, :*, :*, :*, :*, :*, :*, :*, :*]]
    @vs.s.should == [[:*, "VW", "90-120", "< 2 l", "< 180", "Diesel", "< 6 l", "Minivan", "8", :*, :*]]

    @vs.negative_example(["2-5 Jahre", "Peugeot", "75-90", "< 2 l", "< 180", "Super", "< 8 l", "kompakt", "5", "silber/grau", "< 7500"])
    @vs.g.should == [
      [:*, "VW", :*, :*, :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, "90-120", :*, :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, "< 6 l", :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, "Minivan", :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, :*, "8", :*, :*]
    ]
    @vs.s.should == [[:*, "VW", "90-120", "< 2 l", "< 180", "Diesel", "< 6 l", "Minivan", "8", :*, :*]]

    @vs.negative_example(["5-10 Jahre", "VW", "50-75", "< 1.6 l", "< 180", "Benzin", "< 8", "Kompakt", "5", "rot", "< 7500"])
    @vs.g.should == [
      [:*, "VW", :*, "< 2 l", :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, "90-120", :*, :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, "< 6 l", :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, "Minivan", :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, :*, "8", :*, :*]
    ]
    @vs.s.should == [[:*, "VW", "90-120", "< 2 l", "< 180", "Diesel", "< 6 l", "Minivan", "8", :*, :*]]
    
    @vs.negative_example(["< 2 Jahre", "Smart", "30-50", "< 1 l", "< 135", "Super", "< 5", "Kompakt", "2", "bunt", "< 7500"])
    @vs.g.should == [
      [:*, "VW", :*, "< 2 l", :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, "90-120", :*, :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, "< 6 l", :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, "Minivan", :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, :*, "8", :*, :*]
    ]
    @vs.s.should == [[:*, "VW", "90-120", "< 2 l", "< 180", "Diesel", "< 6 l", "Minivan", "8", :*, :*]]
    
    @vs.negative_example(["< 1 Jahr", "Mercedes", "150-200", "< 2 l", "< 250", "Super", "< 10", "Cabrio", "2", "schwarz", "< 30000"])
    @vs.g.should == [
      [:*, "VW", :*, "< 2 l", :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, "90-120", :*, :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, "< 6 l", :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, "Minivan", :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, :*, "8", :*, :*]
    ]
    @vs.s.should == [[:*, "VW", "90-120", "< 2 l", "< 180", "Diesel", "< 6 l", "Minivan", "8", :*, :*]]
    
    @vs.negative_example(["2-5 Jahre", "BMW", "150-200", "< 2.5 l", "< 250", "Super", "< 8", "Limousine", "5", "grün", "< 20000"])
    @vs.g.should == [
      [:*, "VW", :*, "< 2 l", :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, "90-120", :*, :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, "< 6 l", :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, "Minivan", :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, :*, "8", :*, :*]
    ]
    @vs.s.should == [[:*, "VW", "90-120", "< 2 l", "< 180", "Diesel", "< 6 l", "Minivan", "8", :*, :*]]

    @vs.negative_example(["< 1 Jahr", "Peugeot", "120-150", "< 2.5 l", "< 250", "Diesel", "< 8", "Limousine", "5", "grün", "< 50000"])
    @vs.g.should == [
      [:*, "VW", :*, "< 2 l", :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, "90-120", :*, :*, :*, :*, :*, :*, :*, :*],
      [:*, "VW", :*, :*, :*, "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, "< 2 l", :*, "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, "< 180", "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, "< 6 l", :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, "Minivan", :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, :*, "8", :*, :*]
    ]
    @vs.s.should == [[:*, "VW", "90-120", "< 2 l", "< 180", "Diesel", "< 6 l", "Minivan", "8", :*, :*]]

    @vs.negative_example(["2-5 Jahre", "Ferrari", "> 200", "> 2.5 l", "> 250", "Super", "> 12", "Limousine", "2", "rot", "> 50000"])
    @vs.g.should == [
      [:*, "VW", :*, "< 2 l", :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, "90-120", :*, :*, :*, :*, :*, :*, :*, :*],
      [:*, "VW", :*, :*, :*, "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, "< 2 l", :*, "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, "< 180", "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, "< 6 l", :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, "Minivan", :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, :*, "8", :*, :*]
    ]
    @vs.s.should == [[:*, "VW", "90-120", "< 2 l", "< 180", "Diesel", "< 6 l", "Minivan", "8", :*, :*]]

    @vs.negative_example(["> 10 Jahre", "Porsche", "> 200", "> 2.5 l", "> 250", "Super", "< 12", "Limousine", "2", "gelb", "< 30000"])
    @vs.g.should == [
      [:*, "VW", :*, "< 2 l", :*, :*, :*, :*, :*, :*, :*],
      [:*, :*, "90-120", :*, :*, :*, :*, :*, :*, :*, :*],
      [:*, "VW", :*, :*, :*, "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, "< 2 l", :*, "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, "< 180", "Diesel", :*, :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, "< 6 l", :*, :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, "Minivan", :*, :*, :*],
      [:*, :*, :*, :*, :*, :*, :*, :*, "8", :*, :*]
    ]
    @vs.s.should == [[:*, "VW", "90-120", "< 2 l", "< 180", "Diesel", "< 6 l", "Minivan", "8", :*, :*]]

    @vs.positive_example(["neu", "Mercedes", "> 200", "> 2.5l", "< 250", "Diesel", "< 8", "Kombi", "5", "silber/grau", "< 50000"])
    @vs.g.should == []
    @vs.s.should == [[:*, :*, :*, :*, :*, "Diesel", :*, :*, :*, :*, :*]]

    @vs.should be_terminated
  end

  it "should fail to build a VersionSpace for the example 'Fußball'" do
    @vs = VersionSpace.new([[:*, :*]], [[:_, :_]])
    @vs.positive_example(["rund", "schwarzrot"])
    @vs.positive_example(["rund", "schwarzweiss"])

    @vs.g.should == [[:*, :*]]
    @vs.s.should == [["rund", :*]]
    
    @vs.negative_example(["rund", "blau"])
    
    @vs.g.should == []
    @vs.s.should == []

    @vs.should be_terminated
  end

  describe "#more_general?" do
    it "should return true if the first list is equal to the second" do
      @vs.more_general?(["a"], ["a"]).should be_true
      @vs.more_general?([:*, "b"], ["a", "b"]).should be_true
      @vs.more_general?(["<18", :*, :*, :*, :*, :*, :*], ["19-24", "w", "nein", "0", "keiner", "Selbstaendig", "3000-3999"]).should be_false
    end
  end

  describe "#generalize" do
    it "returns an empty list if passed an empty hypothesis" do
      @vs.generalize([], ["a", "b"]).should == []
    end

    it "generalizes the two passed hypotheses" do
      @vs.generalize(["a"], ["a"]).should == ["a"]
      @vs.generalize([:_], ["a"]).should == ["a"]
      @vs.generalize(["a"], [:_]).should == ["a"]

      @vs.generalize(["a"], ["b"]).should == [:*]
    end
  end

  describe "#get_potential_positions" do
    it "should return a list of positions that can be specialized" do
      # Nothing to specialize
      @vs.get_potential_positions(["rund", "blau"], ["rund", "gelb"], ["rund", "blau"]).should == []

      @vs.get_potential_positions(["rund", :*], ["rund", "gelb"], ["rund", "blau"]).should == [1]


      result = @vs.get_potential_positions(
        [:*, :*, :*],
        ["2-5 Jahre", "Peugeot", "75-90"],
        [:*, "VW", "90-120"]
      )
      result.should == [0, 1, 2]
    end
  end

  describe "#specialize" do
    it "should return a list of specialized hypotheses" do
      @vs.specialize(["rund", :*], ["rund", "gelb"], ["rund", "blau"]).should == [["rund", "blau"]]
      @vs.specialize([:*, :*], ["eckig", "blau"], ["rund", "blau"]).should == [["rund", :*]]
      @vs.specialize([:*, :*, :*], ["eckig", "blau", "groß"], ["rund", "blau", "klein"]).should == [["rund", :*, :*], [:*, :*, "klein"]]


      result = @vs.specialize(
        [:*, :*, :*],
        ["2-5 Jahre", "Peugeot", "75-90"],
        [:*, "VW", "90-120"]
      )
      result.should == [
        [:*, :*, :*], [:*, "VW", :*], [:*, :*, "90-120"]
      ]
    end
  end
end