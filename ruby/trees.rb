#!/usr/bin/env ruby

# first ruby program written myself, 2014-05-12

class Arborist
	attr_accessor :species

	def initialize(species = "regular")
		@species = species
	end

	def list
		if @species.nil?
			puts "Please name one or more species"
		elsif @species.respond_to?("each")
			puts "You listed: \n"
			@species.each do |species|
				puts "#{species} tree"
			end
			puts "\n#{@species.join(", ")}---these are the trees."
		else
			puts "#{species} tree"
		end
	end

end

if __FILE__ == $0
	a = Arborist.new
	a.species = ["oak", "maple", "cherry", "sycamore", "dogwood"]
	a.list
end
