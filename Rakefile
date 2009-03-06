#! ruby
require 'fileutils'
require 'webrick'
require 'date'
require 'net/http'

IS_NIX = PLATFORM !~ /win32/
RAKE = IS_NIX ? "rake" : "rake.bat"

task :test_server => [] do
  Dir.chdir("client"){
    sh "#{RAKE} demo_tankwars"
  }
  cp "./client/bin/tankwars.swf", "./test_server/bin"
  Dir.chdir("router"){
    sh "#{RAKE} build"
  }
  mod_path = File.expand_path("./router")
  Dir.chdir("test_server"){
    sh "erl -pa \"#{mod_path}\" -sname im_node -eval \"application:start(island_manager).\""
  }
end

task :default => [:test_server]
