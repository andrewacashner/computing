#!/usr/bin/env ruby

# trying basics of classes, objects, variables, 2014-05-14

class Book

	def initialize (author_last, author_first, title, location, publisher, year)
		@au_l	= author_last
		@au_f	= author_first
		@ti 	= title
		@loc 	= location
		@pub 	= publisher
		@yr 	= year
	end
	
	attr_reader :au_l, :au_f, :ti, :loc, :pub, :yr

	attr_writer :au_l, :au_f, :ti, :loc, :pub, :yr

	def note_style
		@notes_style = "#{@au_f} #{@au_l}, #{@ti} (#{@loc}: #{@pub}, #{@yr})"
	end

	def biblio_style
		@biblio_style = "#{@au_l}, #{@au_f}. #{@ti}. #{@loc}: #{@pub}, #{@yr}."
	end
end

class Bibliography 

	def initialize(args = [])
		@args = args
	end

	def list_notes
		@args.each do |arg|
			puts arg.note_style
		end
	end

	def list_biblio
		@args.each do |arg|
			puts arg.biblio_style
		end
	end
end

book1 = Book.new("King", "Stephen", "The Stand", "New York", "Putnam", 1978)
book2 = Book.new("Copland", "Aaron", "What to Listen for in Music", "New York", "Signet Classics", 1998)
biblio = Bibliography.new([book1, book2])

puts "Notes style:\n"
biblio.list_notes

puts "\nBibliography style:\n" 
biblio.list_biblio




