package com.croqodile{
    import com.croqodile.Message;
    import com.croqodile.IslandReplica;
    import flash.events.*;
    import flash.utils.ByteArray;
    
    public class ExternalMessage implements Message{
		
		public static const MSG_TYPE_TERM:int = 0;
		public static const MSG_TYPE_SNAPSHOT_REQ:int = 1;
		public static const MSG_TYPE_NORMAL:int = 2;
		public static const MSG_TYPE_HEARTBEAT:int = 3;
		public static const MSG_HEAD_LEN:int = 2 + 8 + 4;

		protected var _timestamp:Number = 0;

		
		public static function parseAll(buf:ByteArray):Array {
			var msgs:Array = [];
			var msgBookmark:int;
			while((buf.length - buf.position) >= MSG_HEAD_LEN){
				msgBookmark = buf.position;
				var type:uint = buf.readShort();
				var time:Number = (buf.readInt() << 32) + buf.readInt();
				var len:uint = buf.readInt();
				if((buf.length - buf.position) >= len){
					var content:ByteArray = new ByteArray();
					buf.readBytes(content, 0, len);
					msgs.push(create(type, time, content));
				}
				else{
					buf.position = msgBookmark;
					break;
				}
			}
			return msgs;
		}


		public static function create(type:uint, time:Number, content:ByteArray):ExternalMessage{
			switch(type){
				case MSG_TYPE_TERM:          throw new Error("Should not receive term message.");
				case MSG_TYPE_SNAPSHOT_REQ:  return new SnapshotRequestMessage(time);
				case MSG_TYPE_NORMAL:        return new ExternalIslandMessage(time, content);
				case MSG_TYPE_HEARTBEAT:     return new HeartbeatMessage(time);
			}
			return null;
		}

		public function ExternalMessage(timestamp:Number):void{
			_timestamp = timestamp;
		}

		public function toBytes():ByteArray{ return null; }
		
		public function executionTime():Number{
			return _timestamp;
		}
		
		public function sequenceNumber():int{
			return 0;
		}
		
		public function toString():String{
			return "ExternalMessage(" + _timestamp + ")";
		}
    }
    
}


