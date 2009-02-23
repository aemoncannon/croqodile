package com.croqodile{
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.*;
    import com.croqodile.util.*;
    import flash.events.*;
    import com.croqodile.serialization.json.JSON;
    
    public /*abstract*/ class IslandReplica extends IslandObject {
		protected var _controller:Controller;
		protected var _internalMessageCounter:int = 0;
		protected var _randGenerator:SeededRandom;
		protected var _islandTime:Number = 0;
		protected var _msgQ:MessageQ;
		
		public function IslandReplica(){
			super(this);
			_randGenerator = new SeededRandom();
			_msgQ = new MessageQ(this);
		}
		
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
			if(msg.executionTime() < time()){
				throw new Error("Executing old message!: " + msg.toString());
			}
			_islandTime = msg.executionTime();
			msg.execute();
		}
		
		public function time():Number {
			return _islandTime;
		}

		public function rand():SeededRandom {
			return _randGenerator;
		}
		
		public function scheduleInternalMessage(msg:InternalMessage):void{
			_internalMessageCounter += 1;
			msg.setSequenceNumber(_internalMessageCounter);
			_msgQ.scheduleInternalMessage(msg);
		}
		
		public function advanceToExternalMessage(msg:ExternalMessage):void {
			_msgQ.advanceToExternalMessage(msg);
		}
		
		public function snapshot():String {
			var data:Object = freeze();
			freezeFundamental(data);
			return JSON.encode(data);
		}
		
		public function initFromSnapshot(snapshot:String):void{
			var data:Object = JSON.decode(snapshot);
			unfreezeFundamental(data);
			unfreeze(data); 
		}
		
		private function freezeFundamental(data:Object):void{
			data.time = time();
			data.internalMessageCounter = _internalMessageCounter;
			data.msgQ = _msgQ.freeze();
			data.randGenerator = _randGenerator.freeze();
		}

		private function unfreezeFundamental(data:Object):void{
			if(_internalMessageCounter > 0 || _islandTime > 0){
				throw new Error("Hey! I'm not a fresh replica!");
			}
			_islandTime = data.time;
			_internalMessageCounter = data.internalMessageCounter;
			_msgQ.unfreeze(data.msgQ);
			_randGenerator.unfreeze(data.randGenerator);
		}

		// Subclass responsibility
		public function freeze():Object { return new Object(); }
		
		// Subclass responsibility
		public function unfreeze(data:Object):void {}
		
		////////////////////////
        // External Interface //
        ////////////////////////
		
		public function sunrise():void {}
		
    }
}


