#! /usr/bin/env ruby

# modes.rb -- Andrew Cashner, 2016/12/03

puts "There are twelve modes according to Zarlino."

puts "Mode I \t   Dorian    \t   Final: D\t Ambitus: D to D\n"
puts "Mode II\t   Hypodorian\t   Final: D\t Ambitus: A to A\n"

puts "{ d e f g a }\n"
puts "{ a, b, c d e }\n"

class Mode
  def initialize (number, name, final, ambitusLower, ambitusUpper)
    @number       = number
    @name         = name
    @final        = final
    @ambitusLower = ambitusLower
    @ambitusUpper = ambitusUpper
  end
  attr_reader :number, :name, :final, :ambitusLower, :ambitusUpper
  attr_writer :number, :name, :final, :ambitusLower, :ambitusUpper
  def to_s
    "Mode #{romanNumeral(number - 1)}\t #{@name}\t Final: #{@final}\t Ambitus: #{@ambitusLower} to #{@ambitusUpper}"
  end
end

def romanNumeral(arabic)
  if arabic < 5
    while arabic > 0
      output_str += 'I'
      arabic -= 1
    end
  end
  return output_str
end

modeI = Mode.new(1, "Dorian", "D", "D", "D")

puts(modeI.to_s)

