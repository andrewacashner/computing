#!/usr/bin/env ruby

# Fizz Buzz, Andrew Cashner, 2017/04/17

class FizzBuzz
    def initialize(min, max)
        @min = min
        @max = max
    end
    def test
        if (@num % 15 == 0) 
            @msg = "FizzBuzz"
        elsif (@num % 3 == 0)
            @msg = "Fizz"
        elsif (@num % 5 == 0)
            @msg = "Buzz"
        else 
            @msg = @num
        end
        print "#{@msg} "
    end
    def iterate
        @num = @min
        while @num < @max
            self.test
            @num += 1
        end
        print "\n"
    end
end

fizzbuzz = FizzBuzz.new(1, 100)
fizzbuzz.iterate
