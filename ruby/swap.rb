#! /usr/bin/env ruby

# Swap two given integers

class Swap
	def initialize(first, second)
		@first = first
		@second = second
		puts("Input: #{@first}, #{@second}")
	end

	def do
		@first = @first + @second
		@second = @first - @second
		@first = @first - @second
		puts("Output: #{@first}, #{@second}")
	end
end

myswap = Swap.new(5,8)
myswap.do
