#!/usr/bin/env ruby

# madlibs, first try, Andrew Cashner 2014-05-24

class Interview

	def ask(query)
		@query = query 
		print "#{@query.capitalize}: "
		@input = STDIN.gets.chomp!
	end

	def result
		puts "MADLIB: Answer the questions."
		@fields = [ "noun (person's name)", "noun (body part)",
								"noun (food)" ]
		@name = ask(@fields[0])
		@body_part = ask(@fields[1])
		@food = ask(@fields[2])
		puts "#{@name.capitalize}'s #{@body_part} is full of #{@food}."
	end

end

madlib = Interview.new
madlib.result


#A[n] adj noun-animal was [verb]ing through [noun-place] when he met [name].
#The noun-animal said, [name], your [noun-body part] is full of [noun-food].
#
# adj, noun-animal, verb-movement noun-place name
# noun-animal$1 name$1, noun-bodypart noun-food


