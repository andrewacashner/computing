class Song

	@@plays = 0

	def initialize(name, artist, duration)
		@name 	= name
		@artist	= artist
		@duration = duration
		@plays 	= 0
	end

	attr_reader :name, :artist, :duration

	def to_s
		"#{@name} -- #{@artist} (#{@duration})"
	end

	def play
		@plays +=1
		@@plays += 1
		"This song: #@plays plays. Total #@@plays plays."
	end
end

class Logger
	private_class_method :new
	@@logger = nil

	def Logger.create
		@@logger = new unless @@logger
		@@logger
	end
end

class SongList
	def initialize
		@songs = Array.new
	end

	MaxTime = 5*60	# 5 minutes

	def SongList.isTooLong(aSong)
		return aSong.duration > MaxTime
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

	def [](key)
		return @songs[key] if key.kind_of?(Integer)
		return @songs.find { |aSong| (aSong.name == key) || 
										(aSong.artist == key) }
	end
end
