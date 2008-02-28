#!ruby

COMPILE_COMMAND = "mxmlc -compiler.warn-no-type-decl=false -compiler.source-path  c:/my_system/src/as_dump/as3 c:/flex2/frameworks/source c:/my_system/src/as_dump/as3 c:/my_system/src/misc/croqodile/trunk/src/client  -o=workspace.swf  -file-specs=workspace.mxml"
SWF_TO_RUN = "workspace.swf"

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


IO.popen("fcsh.exe 2>&1", "w+"){|f|
  read_to_prompt(f)
  f.puts COMPILE_COMMAND

  # Begin interaction:
  while true
    output = read_to_prompt(f)

    if output =~ /^#{SWF_TO_RUN} \([0-9]/
        system "SAFlashPlayer.exe #{SWF_TO_RUN}"
    end

    puts "('c' to recompile, 'q' to quit): "
    action = gets
    case action
    when /^c/
      f.puts "compile 1"
    when /^q/
      f.puts "quit"
      break
    else
      f.puts action
    end
  end
}
