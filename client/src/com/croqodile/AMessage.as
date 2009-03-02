package com.croqodile{
    import com.croqodile.Message;
    
    public class AMessage implements Message{

		/*
		* Return true if this sorts before msg.
		*/
		public function sortsBefore(msg:Message):Boolean{
			return this.time < msg.time;
		}

		function get time():Number { return 0; }
		function execute(IslandReplica):void { }
		function toString():String { return "Message"; }
	}

}

