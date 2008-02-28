import flash.display.*;
import mx.controls.textClasses.TextRange;
import flash.ui.Keyboard;
import flash.ui.Mouse;
import flash.utils.getTimer;
import com.senocular.utils.Output;
import com.croqodile.*;
import com.croqodile.workspace.*;
import com.croqodile.events.*;
import com.croqodile.di.*;
import flash.events.*;
import mx.core.*;

private var _controller:Controller;

/* Use this carefully; unless necessary,
   prefer a FarRef for accessing the island. */
private var _island:WorkspaceIsland;


private var _islandRef:FarRef;
private var _avatarRef:FarRef;
private var _canvas:Sprite;
private static const MOUSE_MOVE_FREQ:Number = 50;
private var _lastMoveTime:Number = 0;

private static const CHAT_KEY_CODE:uint = 84; // 't'

public function init():void{
    this.stage.addChild(new Output());
    this.stage.frameRate = 30;
    this.stage.addEventListener(Event.RESIZE, onStageResize);
    this.routerHostInput.text = "localhost";
    
    var canvasComp:UIComponent = new UIComponent();
    canvasComp.x = 0;
    canvasComp.y = 0;
    canvasComp.width = this.stage.stageWidth;
    canvasComp.height = this.stage.stageHeight;
    this._canvas = new Canvas(0, 50, this.stage.stageWidth, this.stage.stageHeight);
    canvasComp.addChild(this._canvas);
    this.addChild(canvasComp);
    
    this.chatInput.focusEnabled = false;
}

private function onStageResize(event:Event):void {
    Canvas(this._canvas).setDimensions(0, 50, this.stage.stageWidth, this.stage.stageHeight);
}

public function onConnectButtonClicked():void {
    var host:String = this.routerHostInput.text;              
    
    /* This is where the magic happens. Instantiating
       the system will automatically  trigger a connection
       to the router.*/
    var config:Object = DIRunner.run([
    {name: "island", klass: WorkspaceIsland, 
     args: {canvas: this._canvas},
     injectArgs: {} },
    {name: "controller", klass: SnapshottingController, 
     args: {policyHost: host, routerHost: host, snapshotHost: host}, 
     injectArgs: {island: "island"}} ]);
    
    this._controller = config.controller;
    this._controller.addEventListener(RouterConnectionReadyEvent.type, routerConnectionReady);
    this.routerHostInput.visible = false;
    this.connectButton.visible = false;
    
    Mouse.hide();
}


public function routerConnectionReady(event:Event):void{
    Output.trace("Router connection ready.");
    this._island = WorkspaceIsland(this._controller.island());
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
	this._canvas.addEventListener(MouseEvent.MOUSE_MOVE, onMouseMove);
	this._canvas.addEventListener(MouseEvent.DOUBLE_CLICK, onDoubleClick);
	this._canvas.addEventListener(MouseEvent.MOUSE_DOWN, onMouseDown);
	this._canvas.addEventListener(MouseEvent.MOUSE_UP, onMouseUp);
	this.stage.addEventListener(KeyboardEvent.KEY_DOWN, onKeyDown);
    }
}

private function onMouseMove(evt:MouseEvent){
    var time:Number = getTimer();
    if((time - this._lastMoveTime) > MOUSE_MOVE_FREQ){
	this._avatarRef.send("moveTo", [this._canvas.mouseX, this._canvas.mouseY]);
	this._lastMoveTime = time;
    }
}

private function onMouseDown(evt:MouseEvent){
    this._avatarRef.send("grab", [this._canvas.mouseX, this._canvas.mouseY]);
}

private function onMouseUp(evt:MouseEvent){
    this._avatarRef.send("release", [this._canvas.mouseX, this._canvas.mouseY]);
}

private function onDoubleClick(evt:MouseEvent){
    Output.trace("double click");
    Output.trace(this._canvas.doubleClickEnabled);
}


private function onKeyDown(evt:KeyboardEvent):void{
    evt.stopPropagation();
    evt.preventDefault();
    
    switch(evt.keyCode){
	
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



