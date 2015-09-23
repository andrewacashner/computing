#!/usr/bin/env ruby

# MADLIB v0.1, Andrew Cashner, 2014-05-25
# take story from string input, extract blanks to be filled in, written in story as {part of speech (description}; query user for answer to each blank; substitute answers back into story and print results
# STDIN/OUT, string input

class Madlib
	
	def initialize(story)
		@story = story
	end

	def Madlib.main(a)
		puts "MADLIB: Fill in the blanks."

		while true 
			
			a.get_blanks
			a.interview
			a.result
		
			print "Try another? [y/n]: "
			@answer = STDIN.gets.chop!
			if @answer != "y" 
				break
			end	

		end

	end

	def get_blanks
		# Find words from input story enclosed in curly brackets
		@pattern = /\{\w*\}/
		# Story successive words found into array
		@blanks = @story.scan(@pattern)
	end

	def ask(query)
		# Remove { and } from query
		@query = query
		@query[0] = '' 
		@query = @query.chop!.capitalize

		# Ask user to fill in the blank 
		print "#{@query}: "

		# Get user input from stdin, chop off newline
		@input = STDIN.gets.chop!
	end

	def interview
		# For as many blanks as are in the story, ask user to fill in each
		# one and store their answers in subsequent elements of new array
		@answers = Array.new
		@blanks.each_with_index{ |blank, index| @answers[index] = ask(blank) }
	end

	def result
		# DEBUG: @answers.each{|word| puts word}	

		# Print madlib story to STDOUT with {x} blanks replaced with answers
		@new_story = @story.gsub(@pattern, "X")
		@answers.each { |answer| @new_story = @new_story.sub("X", answer) }
		puts @new_story
	end
		
end

story = "The {noun} did {verb} his {noun}."
madlib = Madlib.new(story)
Madlib.main(madlib)

