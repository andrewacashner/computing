#!/usr/bin/env ruby

=begin
	helloworld.rb - Ruby/GTK first sample script.
	from http://ruby-gnome2.sourceforge.jp/hiki.cgi?tut-gtk-helloworld
=end

require 'gtk2'

button = Gtk::Button.new("Hello World")
button.signal_connect("clicked") {
	puts "Hello World"
}

window = Gtk::Window.new
window.title = "Hello"
window.signal_connect("delete-event") { 
	puts "delete event occurred"
	#true
	false
}

window.signal_connect("destroy") {
	puts "destroy event occurred"
	Gtk.main_quit
}

window.border_width = 10
window.add(button)
window.show_all

Gtk.main
