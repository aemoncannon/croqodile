package com.croqodile{
    import com.croqodile.Message;
    
    public class AMessage implements Message{

		protected var _timestamp:Number = 0;

		/*
		* Return true if this sorts before msg.
		*/
		public function sortsBefore(msg:Message):Boolean{
			return this.time < msg.time;
		}

		public function get time():Number { return 0; }
		public function execute(IslandReplica):void { }
		public function toString():String { return "Message"; }
	}

}

