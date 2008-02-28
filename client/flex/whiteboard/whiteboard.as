import flash.display.*;
import flash.ui.Keyboard;
import flash.utils.getTimer;
import com.senocular.utils.Output;
import com.croqodile.*;
import com.croqodile.whiteboard.*;
import com.croqodile.events.*;
import com.croqodile.di.*;
import flash.events.*;

private var _controller:Controller;
private var _islandRef:FarRef;
private var _lastX:Number;
private var _lastY:Number;
private static const SEGMENT_FREQ:Number = 50;
private var _lastSegTime:Number = 0;           

public function onConnectButtonClicked():void {
    var host:String = this.routerHostInput.text;              
    
    var config:Object = DIRunner.run([
    {name: "island", klass: WhiteboardIsland, 
     args: {canvas: this.canvas},
     injectArgs: {} },

    {name: "controller", klass: SnapshottingController, 
     args: {policyHost: host, routerHost: host, snapshotHost: host}, 
     injectArgs: {island: "island"}} ]);

    this._controller = config.controller;
    this._controller.addEventListener(RouterConnectionReadyEvent.type, routerConnectionReady);
    this.stage.addChild(new Output());             
}

public function routerConnectionReady(event:Event):void{
    Output.trace("Router connection ready.");
    this._islandRef = this._controller.island().farRef();
    this._controller.addEventListener(DisconnectedFromRouterEvent.type, disconnectedFromRouter);
    this.canvas.addEventListener(MouseEvent.MOUSE_DOWN, onPenDown);
    this.canvas.addEventListener(MouseEvent.MOUSE_UP, onPenUp);
}

public function disconnectedFromRouter(event:Event):void{
    Output.trace("Disconnected from router... :(");
}

private function onPenDown(event:MouseEvent):void{
    this._lastX = event.localX;
    this._lastY = event.localY;
    this.canvas.addEventListener(MouseEvent.MOUSE_MOVE, onPenDraw);
}

private function onPenUp(event:MouseEvent):void{
    this.canvas.removeEventListener(MouseEvent.MOUSE_MOVE, onPenDraw);
}

private function onPenDraw(event:MouseEvent):void{
    var time:Number = getTimer();
    if((time - this._lastSegTime) > SEGMENT_FREQ){
	var segment:Object = new Object();
	segment.startX = this._lastX;
	segment.startY = this._lastY;
	segment.endX = event.localX;
	segment.endY = event.localY;
	segment.color = this.colorPicker.selectedColor; 
	segment.thickness = this.thicknessSlider.value; 
	this._islandRef.send("addSegment", [ segment ]);
	this._lastX = event.localX;
	this._lastY = event.localY;
	this._lastSegTime = time;
    }
}

