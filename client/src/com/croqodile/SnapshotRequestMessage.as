package com.croqodile{
    import flash.events.*;
    
    public class SnapshotRequestMessage extends ExternalMessage{
		
		public function SnapshotRequestMessage(timestamp:Number):void{
			super(timestamp);
		}
		
		
		override public function toString():String {
			return "SnapshotRequestMessage(" + _timestamp + ")";
		}
    }
    
}


