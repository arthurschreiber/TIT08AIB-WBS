require File.join(File.dirname(__FILE__), '..', 'lib', 'data_set')

describe DataSet do
  before(:each) do
    @dataset = DataSet.new(File.join(File.dirname(__FILE__), 'test_input.csv'))
  end
  
  it "should contain all lines, without the file header and without empty lines" do
    @dataset.lines.should == [
      ["0", "19-24", "m", "nein", "0", "keiner", "Angestellter", "3000-3999", "Buch_C"],
      ["1", "25-35", "m", "nein", "1", "Realschule", "Arbeitslos", "1000-1999", "Buch_C"],
      ["2", "36-49", "m", "nein", "0", "Hochschule", "Angestellter", "3000-3999", "Buch_C"],
      ["3", "25-35", "m", "ja", "1", "Hochschule", "Fuehrungskraft", "5000 und mehr", "Buch_C"]
    ]
  end
  
  it "should correctly group the lines" do
    @dataset.grouped_examples.should == {
      "Buch_C" => [
        ["0", "19-24", "m", "nein", "0", "keiner", "Angestellter", "3000-3999", "Buch_C"],
        ["1", "25-35", "m", "nein", "1", "Realschule", "Arbeitslos", "1000-1999", "Buch_C"],
        ["2", "36-49", "m", "nein", "0", "Hochschule", "Angestellter", "3000-3999", "Buch_C"],
        ["3", "25-35", "m", "ja", "1", "Hochschule", "Fuehrungskraft", "5000 und mehr", "Buch_C"]
      ]
    }
  end
end