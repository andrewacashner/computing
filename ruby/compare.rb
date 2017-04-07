#!/usr/bin/env ruby

# Compare two numbers (2014-05-15)

class Comparison
	def initialize(numbers = Array.new)
		@numbers = numbers 
	end
	
	def displayInput
		puts "Compare #{@numbers.join(", ")}"
	end

	def size(type, arrayMember1, arrayMember2)
		@type = type
		@a = @numbers[arrayMember1]
		@b = @numbers[arrayMember2]

		if @type == "greater"
			return @a > @b ? @a : @b
		elsif @type == "less"
			return @a < @b ? @a : @b
		else 
			return "Error: Select \"greater\" or \"less\""
		end
	end

	def greaterThanPi
		@numbers.each do |num|
			if num > 3.14159
				@result = "greater"
			else
				@result = "less"
			end
			puts "#{num} is #{@result} than pi (3.14159)"
		end
		return nil
	end

	def Comparison.testRun(aComparison)
		aComparison.displayInput
		puts "Greater number = #{aComparison.size("greater", 0, 1)}"
		puts "Lesser number = #{aComparison.size("less", 0, 1)}"
		puts "Best number = #{aComparison.size("best", 0, 1)}"
		aComparison.greaterThanPi
	end

end

trial1 = Comparison.new([1, 5])
Comparison.testRun(trial1)
puts "\n"

trial2 = Comparison.new([345*197, 3.45/7890 + 16* 123])
Comparison.testRun(trial2)

puts "\n"
trial3 = Comparison.new([3.14, -34.8])
Comparison.testRun(trial3)

puts "\n"
trial4 = Comparison.new([1, 3, 2, 4, 3, 5])
trial4.displayInput
puts "Smaller of index 0 and 1?"
puts trial4.size("less", 0, 1)
puts "Greater of index 4 and 2?"
puts trial4.size("greater", 4, 2)
trial4.greaterThanPi
