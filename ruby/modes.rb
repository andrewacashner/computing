#! /usr/bin/env ruby

# modes.rb -- Andrew Cashner, 2016/12/03

# puts "There are twelve modes according to Zarlino."

# puts "Mode I \t   Dorian    \t   Final: D\t Ambitus: D to D\n"
# puts "Mode II\t   Hypodorian\t   Final: D\t Ambitus: A to A\n"

# puts "{ d e f g a }\n"
# puts "{ a, b, c d e }\n"

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

def composeRomanNumeral(arabic, referenceNum, romanChar)
  roman = romanChar
  diff = arabic - referenceNum
  if diff == -1
    roman = 'I' + romanChar
  else
    while diff > 0
      roman += 'I'
      diff -= 1
    end
  end
  return roman
end

# 1 5 10 50 100 500 1000
# I V X  L  C   D   M

def romanNumeral(arabic)
  incrementValue = { 1    => 'I',
                     5    => 'V',
                     10   => 'X',
                     50   => 'L',
                     100  => 'C',
                     500  => 'D',
                     1000 => 'M' }
  if arabic > 0
    while incrementvalue[index].key <= 1000
      index = 1
      if (arabic < incrementValue[index].key - 1 and
          arabic < incrementValue[index - 1].key - 1)
        roman = composeRomanNumeral(arabic, incrementValue[index].key,
                                    incrementvalue[index].value)
      else
        index += 1
      end
    end
  end
  
  if arabic <= 12
    if arabic >= 9
      roman = composeRomanNumeral(arabic, 10, 'X')
    elsif arabic < 9 and arabic >= 4
      roman = composeRomanNumeral(arabic, 5, 'V')
    elsif arabic < 4 and arabic > 0
      roman = composeRomanNumeral(arabic, 1, 'I')
    end
    return roman
  end
end

# modeI = Mode.new(1, "Dorian", "D", "D", "D")
# puts(modeI.to_s)

num = 0
while num <= 12
  puts romanNumeral(num)
  num += 1
end

