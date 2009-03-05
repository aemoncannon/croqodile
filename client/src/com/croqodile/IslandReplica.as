package com.croqodile{
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.*;
    import com.croqodile.util.*;
    import flash.events.*;
    import flash.utils.*;
    
    public /*abstract*/ class IslandReplica extends IslandObject {
		protected var _controller:Controller;
		protected var _randGenerator:SeededRandom;
		protected var _islandTime:Number = 0;
		protected var _islandObjDict:Dictionary;
		protected var _msgQ:MessageQ;
		protected var _curGuid:int = 0;
		protected var _islandId:String;
		
		public function IslandReplica(islandId:String){
			_islandId = islandId;
			_randGenerator = new SeededRandom();
			_msgQ = new MessageQ(this);
			_islandObjDict = new Dictionary();
			super(this);
		}

		public function internIslandObject(obj:IslandObject, guid:String = null):IslandObject{
			var g:String = guid || nextGuid();
			_islandObjDict[g] = obj;
			return obj;
		}

		public function islandObjectByGuid(guid:String):IslandObject{
			return IslandObject(_islandObjDict[guid]);
		}

		public function islandObjectByRef(ref:FarRef):IslandObject{
			return islandObjectByGuid(ref.guid());
		}

		public function nextGuid():String{
			_curGuid += 1;
			return String(_curGuid);
		}

		public function get id():String{ return _islandId; }
		
		protected function signalEvent(event:Event):void{
			_controller.signalIslandEvent(event);
		}
		
		public function setController(controller:Controller):void{
			_controller = controller;
		}
		
		public function controller():Controller{
			return _controller;
		}
		
		public function executeMessage(msg:Message):void{
			if(msg.time < this.time){
				throw new Error("Executing old message!: " + msg.toString());
			}
			_islandTime = msg.time;
			msg.execute(this);
		}
		
		public function get time():Number {
			return _islandTime;
		}

		public function rand():SeededRandom {
			return _randGenerator;
		}
		
		public function scheduleInternalMessage(msg:InternalMessage):void{
			_msgQ.scheduleInternalMessage(msg);
		}
		
		public function advanceToExternalMessage(msg:ExternalMessage):void {
			_msgQ.advanceToExternalMessage(msg);
		}
		
		public function snapshot():ByteArray {
			var b:ByteArray = new ByteArray();
			writeFundamentalTo(b);
			writeTo(b);
			b.position = 0;
			return b;
		}
		
		public function initFromSnapshot(snapshot:ByteArray):void{
			readFundamentalFrom(snapshot);
			readFrom(snapshot);
			snapshot.position = 0;
		}
		
		private function readFundamentalFrom(b:IDataInput):void{
			if(_islandTime > 0){
				throw new Error("Hey! I'm not a fresh replica!");
			}
			_curGuid = b.readUnsignedInt();
			_islandTime = b.readDouble();
			_msgQ.readFrom(b);
			_randGenerator.readFrom(b);
		}

		private function writeFundamentalTo(b:IDataOutput):void{
			b.writeUnsignedInt(_curGuid);
			b.writeDouble(_islandTime);
			_msgQ.writeTo(b);
			_randGenerator.writeTo(b);
		}

		protected function readFrom(b:IDataInput):void { throw new Error("Subclass responsibility."); }

		protected function writeTo(b:IDataOutput):void { throw new Error("Subclass responsibility."); }
		
		
		////////////////////////
        // External Interface //
        ////////////////////////
		
		public function sunrise():void { throw new Error("Subclass responsibility."); }
		
    }
}


