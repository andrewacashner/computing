class SongList
	def initialize
		@songs = Array.new
	end
	
	def append(aSong)
		@songs.push(aSong)
		self
	end

	def deleteFirst
		@songs.shift
	end

	def deleteLast
		@songs.pop
	end
end
