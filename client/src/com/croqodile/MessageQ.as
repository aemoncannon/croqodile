package com.croqodile{
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.Controller;
    import com.croqodile.ExternalMessage;
    import flash.events.*;
    import flash.utils.*;
    
    public class MessageQ {
		
		private var _island:IslandReplica;
		private var _msgArray:Array = [];
		
		public function MessageQ(island:IslandReplica){
			_island = island;
		}

		public function get length():int{
			return _msgArray.length;
		}

		public function readFrom(b:IDataInput):void{
			_msgArray = [];
			var msgCount:uint = b.readUnsignedInt();
			for(var i:int = 0; i < msgCount; i++){
				_msgArray.push(InternalMessage.readFrom(b));
			}
		}
		
		public function writeTo(b:IDataOutput):void{
			b.writeUnsignedInt(_msgArray.length);
			for each(var m:InternalMessage in _msgArray){
				m.writeTo(b);
			}
		}
		
		public function scheduleInternalMessage(msg:InternalMessage):void {
			sortIntoQ(msg);
		}
		
		public function advanceToExternalMessage(msg:ExternalMessage):void {
			executeUpToAndIncluding(msg);
		}
		
		/**
		* Starting at the end of the queue, locate the first existing message which
		* 'sortsBefore' msg. Insert msg immediately after that message.
		*
		* Examples:
		* msg = "5"
		* q  = [112222234]
		* q' = [112222234"5"]
		*
		* msg = "5"
		* q  = [112222234555667]
		* q' = [112222234555"5"667]
		*
		* msg = "1"
		* q  = [112222234555667]
		* q' = [11"1"2222234555667]
		*
		* msg = "0"
		* q  = [112222234555667]
		* q' = ["0"112222234555667]
		* 
		* @param msg 
		* @return 
		*/
		private function sortIntoQ(msg:Message):void{
			if(msg.time < _island.time){
				throw new Error("Message time is in the past!: " + msg.toString());
			}
			for(var i:int = _msgArray.length - 1; i > -1; i--){
				var curMsg:Message = _msgArray[i];
				if(curMsg.sortsBefore(msg)){
					_msgArray.splice(i + 1, 0, msg);
					return;
				}
			}
			_msgArray.splice(0, 0, msg);
		}

		
		/**
		* Execute and discard all messages in the queue that 'sortsBefore' msg.
		* Then execute msg.
		*
		* msg = "5"
		* q  = [112222234555667]
		* q' = [667]
		*
		*
		* msg = "0"
		* q  = [112222234555667]
		* q' = [112222234555667]
		*
		*
		* @param msg 
		* @return 
		*/		
		private function executeUpToAndIncluding(msg:Message):void {
			var curMsg:Message = null;
			while((_msgArray.length > 0) && _msgArray[0].sortsBefore(msg)){
				curMsg = _msgArray.shift();
				_island.executeMessage(curMsg);
			}
			_island.executeMessage(msg);
		}

		
		public function toString():String {
			return _msgArray.map(function(ea:InternalMessage, i:int, a:Array):String{
					return ea.toString();
				}).join(",\n");
		}
		
    }
}


