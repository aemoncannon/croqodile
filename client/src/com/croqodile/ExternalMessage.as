package com.croqodile{
    import com.croqodile.Message;
    import com.croqodile.IslandReplica;
    import flash.events.*;
    import flash.utils.ByteArray;
    
    public class ExternalMessage implements Message{
		
		public static const MSG_TYPE_TERM:uint = 0;
		public static const MSG_TYPE_SNAPSHOT_REQ:uint = 1;
		public static const MSG_TYPE_NORMAL:uint = 2;
		public static const MSG_TYPE_HEARTBEAT:uint = 3;

		public static const MSG_HEAD_LEN:int = 1 + 8 + 4;

		protected var _timestamp:Number = 0;

		
		public static function parseAll(buf:ByteArray):Array {
			var msgs:Array = [];
			var msgBookmark:int = buf.position;
			while((buf.length - buf.position) >= MSG_HEAD_LEN){
				msgBookmark = buf.position;
				var type:uint = buf.readUnsignedByte();
				var time:Number = (Number(buf.readUnsignedInt()) * Math.pow(2, 32)) + Number(buf.readUnsignedInt());
				var len:uint = buf.readUnsignedInt();
				if((buf.length - buf.position) >= len){
					var content:ByteArray = new ByteArray();
					buf.readBytes(content, 0, len);
					msgs.push(create(type, time, content));
				}
				else{
					break;
				}
			}
			buf.position = msgBookmark;
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


