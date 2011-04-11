require File.join(File.dirname(__FILE__), '..', 'lib', 'version_space')

describe VersionSpace do
  before :each do
    @vs = VersionSpace.new([[:*, :*, :*]], [[:_, :_, :_]])
  end
  
  it "should be correctly building a version space for a complex example" do
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
  
  describe "#includes?" do
    it "returns true if both parameters are equal" do
      @vs.includes?("a", "a").should be_true
      @vs.includes?("a", "b").should be_false
    end

    it "returns true if the first parameter is more general" do
      @vs.includes?(:*, "a").should be_true
      @vs.includes?(:*, "b").should be_true
    end
  end

  describe "#more_general?" do
    it "should return true if the first list is equal to the second" do
      @vs.more_general?(["a"], ["a"]).should be_true
      @vs.more_general?([:*, "b"], ["a", "b"]).should be_true
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
    end
  end

  describe "#specialize" do
    it "should return a list of specialized hypotheses" do
      @vs.specialize(["rund", :*], ["rund", "gelb"], ["rund", "blau"]).should == [["rund", "blau"]]
      @vs.specialize([:*, :*], ["eckig", "blau"], ["rund", "blau"]).should == [["rund", :*]]
      @vs.specialize([:*, :*, :*], ["eckig", "blau", "groß"], ["rund", "blau", "klein"]).should == [["rund", :*, :*], [:*, :*, "klein"]]
    end
  end
end