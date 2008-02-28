import flash.display.*;
import mx.controls.textClasses.TextRange;
import flash.ui.Keyboard;
import flash.utils.getTimer;
import com.senocular.utils.Output;
import com.croqodile.*;
import com.croqodile.toybox.*;
import com.croqodile.events.*;
import com.croqodile.di.*;
import flash.events.*;
import org.cove.ape.*;
import mx.core.*;

private var _controller:Controller;

/* Use this carefully; unless necessary,
   prefer a FarRef for accessing the island. */
private var _island:ToyboxIsland;


private var _islandRef:FarRef;
private var _avatarRef:FarRef;
private var _APEContainer:Sprite;

private static const CHAT_KEY_CODE:uint = 84; // 't'

public function init():void{
    this.stage.frameRate = 30;
    
    this.routerHostInput.text = "localhost";
    
   
    /* We cannot simply add the Sprite to 'this'. Flex is a bitch, 
       and won't let you add primitive DisplayObjects to any old component. 
       We create a UIComponent to act as surrogate. */
    var canvasComp:UIComponent = new UIComponent();
    canvasComp.x = 0;
    canvasComp.y = 0;
    canvasComp.width = this.stage.stageWidth;
    canvasComp.height = this.stage.stageHeight;
    this._APEContainer = new Sprite();
    this.addChild(canvasComp);
    canvasComp.addChild(this._APEContainer);

    this.stage.addChild(new Output());
    this.chatInput.focusEnabled = false;
}


public function onConnectButtonClicked():void {
    var host:String = this.routerHostInput.text;              

    /* This is where the magic happens. Instantiating
       the system will automatically  trigger a connection
       to the router.*/
    var config:Object = DIRunner.run([
    {name: "island", klass: ToyboxIsland, 
     args: {canvas: this._APEContainer},
     injectArgs: {} },
    {name: "controller", klass: SnapshottingController, 
     args: {policyHost: host, routerHost: host, snapshotHost: host}, 
     injectArgs: {island: "island"}} ]);

    this._controller = config.controller;
    this._controller.addEventListener(RouterConnectionReadyEvent.type, routerConnectionReady);
    this.routerHostInput.visible = false;
    this.connectButton.visible = false;
}


public function routerConnectionReady(event:Event):void{
    Output.trace("Router connection ready.");
    this._island = ToyboxIsland(this._controller.island());
    this._islandRef = this._controller.island().farRef();
    this._controller.addEventListener(DisconnectedFromRouterEvent.type, disconnectedFromRouter);
    
    this._controller.addEventListener(AvatarCreatedEvent.type, onAvatarCreated);
    this._islandRef.send("createAvatar",[this._controller.userId()]);
    
    this.addEventListener(Event.ENTER_FRAME, onEnterFrame);
}


public function disconnectedFromRouter(event:Event):void{
    Output.trace( "Disconnected from router... :(" );
    throw new Error("Lost connection to the router :(");
}


public function onAvatarCreated(event:Event):void{
    var e:AvatarCreatedEvent = AvatarCreatedEvent(event);
    if(this._controller.userId() == e.userId){
	this._avatarRef = e.avatarRef;
	this.stage.addEventListener(KeyboardEvent.KEY_DOWN, onKeyDown);
    }
}


private function onKeyDown(evt:KeyboardEvent):void{
    evt.stopPropagation();
    evt.preventDefault();

    switch(evt.keyCode){
	
    case Keyboard.LEFT:
	this._avatarRef.send("addForce", [-5,0]);
	break;
	
    case Keyboard.RIGHT:
	this._avatarRef.send("addForce", [5,0]);
	break;

    case Keyboard.UP:
	this._avatarRef.send("addForce", [0,-5]);
	break;
	
    case Keyboard.DOWN:
	this._avatarRef.send("addForce", [0,5]);
	break;

    case Keyboard.DOWN:
	this._avatarRef.send("addForce", [0,5]);
	break;

    case CHAT_KEY_CODE:
	this.clearChatInput();
	this.chatInput.visible = true;
	this.stage.removeEventListener(KeyboardEvent.KEY_DOWN, onKeyDown);
	this.stage.addEventListener(KeyboardEvent.KEY_DOWN, onChatKeyDown);
	break;
    }
}


private function onChatKeyDown(evt:KeyboardEvent):void{
    evt.stopPropagation();

    this.chatInput.setFocus();
    
    switch(evt.keyCode){
	
    case Keyboard.ENTER:
	this._avatarRef.send("sayWords", [this.chatInput.text]);
	this.chatInput.visible = false;
	this.setFocus(); //Get rid of focus
	this.stage.removeEventListener(KeyboardEvent.KEY_DOWN, onChatKeyDown);
	this.stage.addEventListener(KeyboardEvent.KEY_DOWN, onKeyDown);
	break;
    }
}


private function clearChatInput():void{
    var range:TextRange = new TextRange(this.chatInput, false, 0);
    range.text = "";
    range.htmlText = "";
}


private function onEnterFrame(evt:Event):void{
    this._island.render();
}



