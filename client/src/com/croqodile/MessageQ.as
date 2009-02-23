package com.croqodile{
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.Controller;
    import com.croqodile.ExternalMessage;
    import flash.events.*;
    
    public class MessageQ {
		
		private var _island:IslandReplica;
		private var _msgArray:Array = [];
		
		public function MessageQ(island:IslandReplica){
			_island = island;
		}

		public function length():int{
			return _msgArray.length;
		}
		
		public static function sortsBefore(msgA:Message, msgB:Message):Boolean{
			if(msgA.executionTime() != msgB.executionTime()){
				return msgA.executionTime() < msgB.executionTime();
			}
			else if(msgA.sequenceNumber() != msgB.sequenceNumber()){
				return msgA.sequenceNumber() < msgB.sequenceNumber();
			}
			else{
				throw new Error("Simultaneous messages, " + msgA + " and " + msgB + ".");
			}
		}
		
		public function unfreeze(data:Array):void{
			_msgArray = [];
			for each(var datum:Object in data){
				var m:InternalMessage = InternalMessage.unfreeze(datum, _island);
				_msgArray.push(m);
			}
		}
		
		public function freeze():Array{
			var data:Array = [];
			for each(var m:InternalMessage in _msgArray){
				data.push(m.freeze());
			}
			return data;
		}
		
		public function scheduleInternalMessage(msg:InternalMessage):void {
			sortIntoQ(msg);
		}
		
		public function advanceToExternalMessage(msg:ExternalMessage):void {
			executeUpTo(msg);
		}
		
		private function sortIntoQ(msg:Message):void{
			
			if(msg.executionTime() < _island.time()){
				throw new Error("Message out of date!: " + msg.toString());
			}
			
			for(var i:int = _msgArray.length - 1; i > -1; i--){
				var curMsg:Message = _msgArray[i];
				if(MessageQ.sortsBefore(curMsg, msg)){
					_msgArray.splice(i + 1, 0, msg);
					return;
				}
			}
			_msgArray.splice(0, 0, msg);
		}
		
		private function executeUpTo(msg:Message):void {
			var curMsg:Message = null;
			while((_msgArray.length > 0) &&
				MessageQ.sortsBefore(_msgArray[0], msg)){
				curMsg = _msgArray.shift();
				_island.executeMessage(curMsg);
			}
			_island.executeMessage(msg);
		}
		
		public function toString():String {
			return _msgArray.map(function(ea, i, array){
					return ea.toString();
				}).join(",\n");
		}
		
    }
}


