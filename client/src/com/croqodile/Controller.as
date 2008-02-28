package com.croqodile {
    import flash.display.MovieClip;
    import flash.system.Security;
    import com.senocular.utils.Output;
    import flash.utils.Timer;
    import flash.events.*;
    import com.croqodile.*;
    import com.croqodile.events.*;
    

    /* Note: Direction instantiation of this class is deprecated.
       Please see SnapshottingController instead. */
    public class Controller extends EventDispatcher {
	
	protected static const POLICY_SERVER_HOST:String = "localhost";
	protected static const POLICY_SERVER_PORT:int = 5001;
	
	protected var _messageCon:RouterMessageConnection;
	protected var _island:IslandReplica;
	protected var _userId:String;
	protected var _msgBuffer:Array = [];
	
	protected function genUserId():String {
	    return "user" + (new Date()).getTime() + "avatar" + Math.random();
	}
	
	public function userId():String{
	    return this._userId;
	}
	
	public function island():IslandReplica {
	    return this._island;
	}
	
	public function signalIslandEvent(event:Event):void{
	    this.dispatchEvent(event);
	}
	
	public function propagateFarSend(msgString:String):void{
	    this._messageCon.sendSentence(msgString);
	}
	
	public function disconnectFromRouter():void{
	    this._messageCon.disconnect();
	}
	
	///////////////////////////
        //    Event Handlers	 //
        ///////////////////////////
	
	protected function onRouterConnectionReady(event:Event):void {
	    this.dispatchEvent(new RouterConnectionReadyEvent());
	}
	
	protected function onRouterConnectionClosed(event:Event):void {
	    this.dispatchEvent(new DisconnectedFromRouterEvent());
	}
	
	protected function onMessageFromRouter(e:ExternalMessageEvent):void {
	    this._island.advanceToExternalMessage(ExternalMessage.fromRouterString(e.msg, this._island));
	}
	
    }
}


