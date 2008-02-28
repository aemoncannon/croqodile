package com.croqodile{
    import flash.display.MovieClip;
    import flash.display.Sprite;
    import flash.display.DisplayObjectContainer;
    import flash.system.Security;
    import com.senocular.utils.Output;
    import flash.utils.Timer;
    import com.croqodile.*;
    import com.croqodile.util.*;
    import flash.events.*;
    import com.croqodile.serialization.json.JSON;
    
    public class IslandReplica extends IslandObject {
	protected var _controller:Controller;
	protected var _internalMessageCounter:int = 0;
	protected var _randGenerator:SeededRandom;
	protected var _islandTime:Number = 0;
	protected var _msgQ:MessageQ;
	
	public function IslandReplica(){
	    super(this);
	    this._randGenerator = new SeededRandom();
	    this._msgQ = new MessageQ(this);
	}
	
	protected function signalEvent(event:Event):void{
	    this._controller.signalIslandEvent(event);
	}
	
	public function setController(controller:Controller):void{
	    this._controller = controller;
	}
	
	public function controller():Controller{
	    return this._controller;
	}
	
	public function executeMessage(msg:Message):void{
	    if(msg.executionTime() < this.time()){
		throw new Error("Executing old message!: " + msg.toString());
	    }
	    this._islandTime = msg.executionTime();
	    msg.execute();
	}
	
	public function time():Number {
	    return this._islandTime;
	}

	public function rand():SeededRandom {
	    return this._randGenerator;
	}
	
	public function scheduleInternalMessage(msg:InternalMessage):void{
	    this._internalMessageCounter += 1;
	    msg.setSequenceNumber(this._internalMessageCounter);
	    this._msgQ.scheduleInternalMessage(msg);
	}
	
	public function advanceToExternalMessage(msg:ExternalMessage):void {
	    this._msgQ.advanceToExternalMessage(msg);
	}
	
	public function snapshot():String {
	    var data:Object = this.freeze();
	    this.freezeFundamental(data);
	    return JSON.encode(data);
	}
	
	public function initFromSnapshot(snapshot:String):void{
	    var data:Object = JSON.decode(snapshot);
	    this.unfreezeFundamental(data);
	    this.unfreeze(data); 
	}
	
	private function freezeFundamental(data:Object):void{
	    data.time = this.time();
	    data.internalMessageCounter = this._internalMessageCounter;
	    data.msgQ = this._msgQ.freeze();
	    data.randGenerator = this._randGenerator.freeze();
	}

	private function unfreezeFundamental(data:Object):void{
	    if(this._internalMessageCounter > 0 || this._islandTime > 0){
		throw new Error("Hey! I'm not a fresh replica!");
	    }
	    this._islandTime = data.time;
	    this._internalMessageCounter = data.internalMessageCounter;
	    this._msgQ.unfreeze(data.msgQ);
	    this._randGenerator.unfreeze(data.randGenerator);
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


