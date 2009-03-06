package com.croqodile{
    import com.croqodile.Message;
    
    public class AMessage extends Obj implements Message{

		protected var _timestamp:Number = 0;

		/*
		* Return true if this sorts before msg.
		*/
		public function sortsBefore(msg:Message):Boolean{
			return this.time <= msg.time;
		}

		/*
		* Return true if this sorts before t.
		*/
		public function sortsBeforeTime(t:Number):Boolean{
			return this.time <= time;
		}

		public function get time():Number { return _timestamp; }

		public function execute(island:IslandReplica):void { }

		public function toString():String { return "Message"; }

		override public function equals(o:Object):Boolean{
			return (o is AMessage && this.time == o.time);
		}
	}

}

