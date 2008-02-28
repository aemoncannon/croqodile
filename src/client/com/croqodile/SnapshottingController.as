package com.croqodile {
    import flash.display.MovieClip;
    import flash.system.Security;
    import com.senocular.utils.Output;
    import flash.utils.Timer;
    import flash.events.*;
    import com.croqodile.*;
    import com.croqodile.events.*;
    
    public class SnapshottingController extends Controller {
	
	protected var _snapshotCon:RouterSnapshotConnection;
	
	/* The chain of events set in motion by instantiating this controller is as follows.
	   
	   1) A security policy is downloaded and installed.
	   
	   2) We create an instance of the island, give it a reference to this controller.
	   
	   3) We create a connection to the message server. Messages will begin
	   flowing immediately, but we do not execute them - we buffer them until later.
	   
	   4) We create a connection to the snapshot server, this constitutes a request
	   for a snapshot.
	   
	   5) The snapshot server replies. If we are the first to connect, then we don't need a snapshot, 
	   execute all of our buffered messages, and continue as per normal - 
	   our first action being to send 'sunrise' to the island.
	   
	   Otherwise, if we are NOT the first to connect, and there are no problems, we will
	   receive a snapshot. We immediately install the snapshot to the island. If any of our 
	   buffered messages are older than the new island-time, we discard them, and then
	   begin executing normally. */

	function SnapshottingController(config:Object){
	    super();
	    
	    config.policyHost = (config.policyHost ? config.policyHost : POLICY_SERVER_HOST);
	    config.policyPort = (config.policyPort ? config.policyPort : POLICY_SERVER_PORT);
	    
	    this._userId = this.genUserId();
	    
	    this._messageCon = (config.messageCon ? config.messageCon : new RouterMessageConnection({}));
	    this._snapshotCon = (config.snapshotCon ? config.snapshotCon : new RouterSnapshotConnection({}));
	    
	    this._island = config.island;
	    this._island.setController(this);
	    
	    this._messageCon.addEventListener(RouterConnection.CONNECTION_CLOSED,
					      this.onRouterConnectionClosed);
	    this._messageCon.addEventListener(ExternalMessageEvent.type,
					      this.onMessageFromRouterBuffered);
	    
	    this._snapshotCon.addEventListener(SnapshotReceivedEvent.type,
					       this.onSnapshotReceived);
	    this._snapshotCon.addEventListener(RouterSnapshotConnection.NO_SNAPSHOT_AVAILABLE,
					       this.onNoSnapshotAvailable);
	    this._snapshotCon.addEventListener(RouterSnapshotConnection.NO_SNAPSHOT_NECESSARY,
					       this.onNoSnapshotNecessary);
	    this._snapshotCon.addEventListener(RouterSnapshotConnection.SNAPSHOT_REQUESTED,
					       this.onSnapshotRequested);
	    
	    /* Connect to the router! */
	    Security.loadPolicyFile("xmlsocket://" + config.policyHost + ":" + config.policyPort);
	    this._snapshotCon.connect(this._userId);
	    this._messageCon.connect(this._userId);
	    
	}
	
	///////////////////////////
        //    Event Handlers	 //
        ///////////////////////////
	
	protected function onSnapshotRequested(event:Event):void {
	    this._snapshotCon.sendSentence(this._island.snapshot());
	}
	
	protected function onSnapshotReceived(event:SnapshotReceivedEvent):void {
	    this._island.initFromSnapshot(event.snapshot);
	    
	    var time:Number = this._island.time();
	    for each(var m:ExternalMessage in this._msgBuffer){
		    //Timestamps are guaranteed by router to be 
		    //always increasing, never repeating
		    if(m.executionTime() > time){
			this._island.advanceToExternalMessage(m);
		    }
		}
	    this._msgBuffer = [];
	    
	    this._messageCon.removeEventListener(ExternalMessageEvent.type,
						 this.onMessageFromRouterBuffered);
	    this._messageCon.addEventListener(ExternalMessageEvent.type,
					      this.onMessageFromRouter);
	    
	    //Start waiting for the main message connection..
	    this._messageCon.addEventListener(RouterConnection.CONNECTION_READY,
					      this.onRouterConnectionReady);
	    
	    //..might have already connected while we were grabbing the snapshot, so..
	    if(this._messageCon.ready()){
		this.onRouterConnectionReady(new Event(RouterConnection.CONNECTION_READY));
	    }
	}
	
	protected function onNoSnapshotAvailable(event:Event):void {
	    throw new Error("Could not get a snapshot :(");
	}
	
	protected function onNoSnapshotNecessary(event:Event):void {
	    for each(var m:ExternalMessage in this._msgBuffer){
		    this._island.advanceToExternalMessage(m);
		}
	    Output.trace("Executed " + this._msgBuffer.length + " buffered messages");
	    
	    this._msgBuffer = [];
	    
	    this._messageCon.removeEventListener(ExternalMessageEvent.type, 
						 this.onMessageFromRouterBuffered);
	    this._messageCon.addEventListener(ExternalMessageEvent.type, 
					      this.onMessageFromRouter);
	    
	    //We are the first controller to connect, so send the island a sunrise :)
	    var self:SnapshottingController = this;
	    var readyHandler:Function = function():void{
		self._island.farRef().send("sunrise", []);
		self.onRouterConnectionReady(new Event(RouterConnection.CONNECTION_READY));
	    }
	    
	    //Start waiting on the main message connection..
	    this._messageCon.addEventListener(RouterConnection.CONNECTION_READY, readyHandler);
	    
	    //..might have already connected while we were asking for the snapshot, so..
	    if(this._messageCon.ready()){ readyHandler(); }
	    
	}
	
	//Buffer the messages while the snapshot is loading..
	protected function onMessageFromRouterBuffered(e:ExternalMessageEvent):void {
	    this._msgBuffer.push(ExternalMessage.fromRouterString(e.msg, this._island));
	}
	
    }
}


