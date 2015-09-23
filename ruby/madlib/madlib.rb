#!/usr/bin/env ruby

# MADLIB v0.3, Andrew Cashner, 2014-05-26

# Take story from string input, extract blanks to be filled in, written in story as {part of speech (description}; query user for answer to each blank; substitute answers back into story and print results

# Changelog
# v0.3 Fixed file I/0, reg. exp
# v0.2 New version using file I/O instead of STDIN/OUT and string input, using single input file for now; cleaned up parsing methods

# options for reading file without having to close it, thanks workmad3 #ruby-lang
# @story = File.read(@infile_name)
# File.open(@infile_name) {|file| @story = File.read}

class Madlib
	
	def initialize(storyname)
		@infile_name = storyname
	end

	def Madlib.main(a)
		puts "\nMADLIB: Fill in the blanks."

		while true 
			a.read_story
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
	end

	def read_story 
		@story = File.read(@infile_name)
		# Read in story file
		# Find words from input story enclosed in curly brackets
		# Story successive words found into array
		@in_brackets = /(?<=\{)[^}]*(?=\})/
		@blanks = @story.scan(@in_brackets)
	end

	def ask(query)
		# Ask user to fill in the blank 
		# Get user input from stdin, chop off newline
		@query = query 
		print "#{@query.capitalize}: "
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
		@outfile = File.write(@outfile_name, @new_story)
	end
end

madlib = Madlib.new("story1.txt")
Madlib.main(madlib)

