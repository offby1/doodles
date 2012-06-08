require 'set'

class Partition
  attr_reader :seq

  # seq must be non-increasing!
  def initialize(seq)
    @seq = seq
  end

  # Find all the partitions that sum to one more than this one.
  def more_partitions
    seqs = Set.new()

    seqs = seqs.add(increment_at(0))

    (1..((@seq.size) - 1)).each do |index|
      if seq[index - 1] > seq[index]
        seqs = seqs.add(increment_at(index))
      end
    end

    seqs = seqs.add(increment_at(@seq.size))

    Set.new(seqs.map {|s| Partition.new(s)})
  end

  def self.all_partitions(n)
    if n == 1
      Set.new([self.new([1])])
    else
      result = Set.new
      self.all_partitions(n - 1).each do |sub_p|
        sub_p.more_partitions.each do |another|
          result << another
        end
      end
      result
    end
  end

  def eql?(other)
    @seq.eql?(other.seq)
  end

  def hash
    @seq.hash
  end

  def to_s
    @seq.join(' + ')
  end

  private
  def increment_at(index)
    result = @seq.flatten       # first way I could come up with to copy the array

    if index < result.size
      result[index] += 1
    else
      result[index] = 1
    end
    result
  end
end

describe Partition do
  context "increment_at" do
    it "should not mutate its input" do
      seq = [3, 2, 1]
      seq.freeze
      p = Partition.new(seq)
      p.seq.should === seq
      p.instance_eval { increment_at(0) }.should == [4, 2, 1]
      p.seq.should === seq
    end
  end

  context "finding more partitions" do
    it "should return [2] and [1, 1] given [1]" do
      p = Partition.new([1])
      Set.new(p.more_partitions.map{|p| p.seq}).should == Set.new([[2], [1, 1]])
    end

    it "should return [3, 1] and [2, 2] given [2, 1]" do
      p = Partition.new([2, 1])
      Set.new(p.more_partitions.map{|p| p.seq}).should == Set.new([[3, 1], [2, 2], [2, 1, 1]])
    end
  end

  context "finding all partitions of an integer" do
    it "should return [1] for 1" do
      Partition.all_partitions(1).should == Set.new([Partition.new([1])])
    end

    it "should return [2] and [1, 1] for 2" do
      Partition.all_partitions(2).should == Set.new([Partition.new([2]), Partition.new([1, 1])])
    end

    it "should return the correct number of partitions for the first few N" do
      # http://en.wikipedia.org/wiki/Partition_(number_theory)#Partition_function
      [1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42].each_with_index do |expected, index|
        if index > 0
          all = Partition.all_partitions(index)
          all.size.should == expected

          # Oh, and a partition of N should add up to N.
          all.each do |p|
            p.seq.reduce(:+).should == index
          end

        end
      end
    end
  end
end

Partition.all_partitions(18).each {|p| puts p}
