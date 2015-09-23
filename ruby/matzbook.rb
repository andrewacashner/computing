def are_you_sure?
	while true
		print "Are you sure? [y/n]: "
		response = gets
		case response
		when /^[yY]/
			return true
		when /^[nN]/, /^$/
			return false
		end
	end
end

class Sequence
	include Enumerable

	def initialize(from, to, by)
		@from, @to, @by = from, to, by
	end

	def each
		x = @from
		while x <= @to
			yield x
			x += @by
		end
	end

	def length
		return 0 if @from > @to
		Integer((@to - @from) / @by) + 1
	end

	alias size length

	def[](index)
		return nil if index < 0
		v = @from + index * @by
		if v <= @to
			v
		else
			nil
		end
	end

	def *(factor)
		Sequence.new(@from * factor, @to * factor, @by * factor)
	end

	def +(offset)
		Sequence.new(@from + offset, @to + offset, @by)
	end
end
