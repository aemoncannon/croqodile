#!ruby
require 'webrick'
include WEBrick
require 'net/http'

COMPILE_COMMAND = "mxmlc -debug=true -compiler.warn-no-type-decl=false -compiler.source-path  c:/my_system/src/as_dump/as3 c:/flex2/frameworks/source c:/my_system/src/as_dump/as3 c:/my_system/src/misc/croqodile/trunk/src/client  -o=workspace.swf  -file-specs=workspace.mxml"

SWF_TO_RUN = "workspace.swf"
PORT = 2000
HOST = "localhost"

############################################
# If a parameter was provided, take action #
############################################

begin
  case ARGV[0]
  when "compile"
    http = Net::HTTP.new(HOST, PORT)
    resp, date = http.get('/compile')
    puts resp.body
    exit
  when "compile_and_show"
    http = Net::HTTP.new(HOST, PORT)
    resp, date = http.get('/compile_and_show')
    puts resp.body
    exit
  when "exit"
    http = Net::HTTP.new(HOST, PORT)
    resp, date = http.get('/exit')
    puts resp.body
    exit
  end
rescue => e
  puts "Command failed: #{e}"
  exit(1)
end       


#################################################################
# Otherwise, if there are no parameters, start the build server #
#################################################################

def read_to_prompt(f)
  output = ""
  while chunk = f.read(1)
    $stdout.write chunk
    output << chunk
    case output
    when /^\(fcsh\) /
      break
    end
  end
  output
end

fcsh = IO.popen("fcsh.exe  2>&1", "w+")
read_to_prompt(fcsh)
fcsh.puts COMPILE_COMMAND
read_to_prompt(fcsh)


#####################################################
# Now expose the shell through a small http server  #
#####################################################

s = HTTPServer.new( :Port => PORT )

s.mount_proc("/compile"){|req, res|
  fcsh.puts "compile 1"
  output = read_to_prompt(fcsh)
  res.body = output
  res['Content-Type'] = "text/html"
}

s.mount_proc("/compile_and_show"){|req, res|
  fcsh.puts "compile 1"
  output = read_to_prompt(fcsh)
  res.body = output
  res['Content-Type'] = "text/html"
  if output =~ /^#{SWF_TO_RUN} \([0-9]/
      system "SAFlashPlayer.exe #{SWF_TO_RUN}"
  end
}

s.mount_proc("/exit"){|req, res|
  s.shutdown
  fcsh.close
  exit
}

trap("INT"){
  s.shutdown 
  fcsh.close
}

s.start

