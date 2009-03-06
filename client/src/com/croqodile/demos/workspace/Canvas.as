package com.croqodile.workspace {
    import flash.display.*;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import flash.text.TextField;
    import flash.text.TextFormat;
    import com.croqodile.*;
    
    public class Canvas extends Sprite {
	
	private var _width:Number;
	private var _height:Number;

	public function Canvas(x:Number, y:Number, width:Number, height:Number){
	    this.doubleClickEnabled = true;
	    this.setDimensions(x, y, width, height);
	}

	public function draw():void{
	    this.graphics.clear();
	    this.graphics.beginFill(0x333333);
	    this.graphics.drawRect(0, 0, this._width, this._height);
	    this.graphics.endFill();
	}
	
	public function setDimensions(x:Number, y:Number, width:Number, height:Number):void{
	    this.x = x;
	    this.y = y;
	    this._width = width;
	    this._height = height;
	    this.draw();
	}
	
	
    }
}

