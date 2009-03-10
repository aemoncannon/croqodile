package com.croqodile{
    import com.croqodile.Message;
    import com.croqodile.IslandReplica;
    import flash.events.*;
    import flash.utils.*;
    
    public class ExternalMessage extends AMessage{
		
		public static const MSG_TYPE_TERM:uint = 0;
		public static const MSG_TYPE_SNAPSHOT_REQ:uint = 1;
		public static const MSG_TYPE_NORMAL:uint = 2;
		public static const MSG_TYPE_HEARTBEAT:uint = 3;
		public static const MSG_HEAD_LEN:int = 1 + 8 + 8 + 4;

		protected var _sequenceNumber:Number = 0;
		
		public static function parseAll(buf:ByteArray):Array {
			var msgs:Array = [];
			var msgBookmark:int = buf.position;
			while((buf.length - buf.position) >= MSG_HEAD_LEN){
				msgBookmark = buf.position;
				var type:uint = buf.readUnsignedByte();
				var num:Number = buf.readDouble();
				var time:Number = buf.readDouble();
				var len:uint = buf.readUnsignedInt();
				if((buf.position + len) < buf.length){
					var content:ByteArray = new ByteArray();
					if(len > 0) buf.readBytes(content, 0, len);
					msgs.push(create(type, num, time, content));
				}
				else{
					break;
				}
			}
			buf.position = msgBookmark;
			return msgs;
		}


		public static function create(type:uint, num:Number, time:Number, content:ByteArray):ExternalMessage{
			switch(type){
				case MSG_TYPE_TERM:          throw new Error("Should not receive term message.");
				case MSG_TYPE_SNAPSHOT_REQ:  return new SnapshotRequestMessage(num, time);
				case MSG_TYPE_NORMAL:        return ExternalIslandMessage.createFromPayload(num, time, content);
				case MSG_TYPE_HEARTBEAT:     return new HeartbeatMessage(num, time);
			}
			throw new Error("Oops, unknown message type: " + type);
			return null;
		}

		public function ExternalMessage(num:Number, timestamp:Number):void{
			_sequenceNumber = num;
			_timestamp = timestamp;
		}

		public function writeTo(b:IDataOutput):void{
			b.writeByte(this.type);
			b.writeDouble(_sequenceNumber);
			b.writeDouble(_timestamp);
			writePayloadTo(b);
		}

		protected function writePayloadTo(b:IDataOutput):void{ }

		public function get sequenceNumber():int{
			return _sequenceNumber;
		}
		
		/* For testing */
		public function unsafeStamp(num:Number, time:Number):void{
			_sequenceNumber = num;
			_timestamp = time;
		}

		protected function get type():uint{ return 0; }
		
		override public function toString():String{
			return "ExternalMessage(" + _timestamp + ")";
		}

		public function toByteArray():ByteArray{ 
			var b:ByteArray = new ByteArray();
			writeTo(b);
			return b;
		}
    }
    
}


