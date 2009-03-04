package com.croqodile{
    import flash.events.*;
    
    public class SnapshotRequestMessage extends ExternalMessage{
		
		public function SnapshotRequestMessage(num:Number, timestamp:Number):void{
			super(num, timestamp);
		}

		override protected function get type():uint{ return MSG_TYPE_SNAPSHOT_REQ; }
		
		
		override public function toString():String {
			return "SnapshotRequestMessage(" + _timestamp + ")";
		}
    }
    
}


