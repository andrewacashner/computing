#!/usr/bin/env ruby

# MADLIB v0.2, Andrew Cashner, 2014-05-26

# Take story from string input, extract blanks to be filled in, written in story as {part of speech (description}; query user for answer to each blank; substitute answers back into story and print results
# New version using file I/O instead of STDIN/OUT and string input, using single input file for now; cleaned up parsing methods

# @story = File.read(@infile_name)
# File.open(@infile_name) {|file| @story = File.read}
class Madlib
	
	def initialize(storyname)
		@infile_name = storyname
		@@infile = File.open(@infile_name, "rb")
		@story = @@infile.read
	end

	def Madlib.main(a)
		puts "\nMADLIB: Fill in the blanks."

		while true 
			a.get_blanks
			a.interview
			a.result
			
			print "\nWould you like to save your story? [y/n]: "
			@answer = STDIN.gets.chop!
			if @answer == "y"
				print "Enter file name (example: story.txt): "
				@outfile_name = STDIN.gets.chop! 
				a.write_story(@outfile_name)
				puts "Story saved as #{@outfile_name}."
			end

			print "\nTry the madlib again? [y/n]: "
			@answer = STDIN.gets.chop!
			if @answer != "y" 
				break
			end	

		end
		
		# FIX: Why does this only work if it's @@infile, not @infile?
		@@infile.close
	end

	def get_blanks
		# Find words from input story enclosed in curly brackets
		# Story successive words found into array
		@in_brackets = /\{([^}]+)\}/
		@blanks = @story.scan(@in_brackets)
		# FIX: multiple-word expressions are stored as an array, so @blanks is an array of arrays. How to story the characters inside brackets as a string, regardless of their contents, and create an array of the strings?
	end

	def ask(blank)
		# Make array take from blank into single string
		@blank = blank 
		@query = @blank.join()

		# Ask user to fill in the blank 
		print "#{@query.capitalize}: "

		# Get user input from stdin, chop off newline
		@input = STDIN.gets.chop!
	end

	def interview
		# For as many blanks as are in the story, ask user to fill in each one and store their answers in subsequent elements of new array
		# FIX: Is this the best way to do this?
		
		@answers = Array.new
		@blanks.each_with_index{ |blank, index| @answers[index] = ask(blank) }
	end

	def result
		# Print madlib story to STDOUT with {x} blanks replaced with answers
		# Capture whole blank including brackets
		# FIX: Why can't I put the regexp into the .sub block directly?

		@brackets = /\{[^}]+\}/
		@new_story = @story.gsub(@brackets, "X")
		@answers.each { |answer| @new_story = @new_story.sub("X", answer) }
		puts "\n", @new_story
	end
	
	def write_story(outfile_name)
		@outfile_name = outfile_name
		@@outfile = File.new(@outfile_name, "w")
		@@outfile.puts @new_story
		@@outfile.close
	end
end

madlib = Madlib.new("story1.txt")
Madlib.main(madlib)

