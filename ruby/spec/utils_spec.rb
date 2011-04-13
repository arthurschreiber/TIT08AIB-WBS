require File.join(File.dirname(__FILE__), '..', 'lib', 'utils')

describe "#more_general?" do
  it "should return true if the first list is equal to the second" do
    more_general?(["a"], ["a"]).should be_true
    more_general?([:*, "b"], ["a", "b"]).should be_true
    more_general?(["<18", :*, :*, :*, :*, :*, :*], ["19-24", "w", "nein", "0", "keiner", "Selbstaendig", "3000-3999"]).should be_false
  end
end
