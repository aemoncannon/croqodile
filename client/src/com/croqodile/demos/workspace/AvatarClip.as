package com.croqodile.workspace {
    import flash.display.*;
    import flash.text.TextField;
    import flash.text.TextFormat;
    
    public class AvatarClip extends MovieClip {
	private var _circle:Shape;
	private var _color:Number = 0xFFFFFF;
	private var _alpha:Number = 0.35;
	
	public function AvatarClip(){
	    this.draw();
	}
	
	private function draw():void{
	    if(this._circle && this.contains(this._circle)){
		this.removeChild(this._circle);
	    }
	    this._circle = new Shape();
	    this._circle.graphics.beginFill(this._color);
	    this._circle.graphics.drawCircle(0, 0, 3);
	    this._circle.x = 0;
	    this._circle.y = 0;
	    this._circle.alpha = this._alpha;
	    this.addChild(this._circle);
	}
	
	public function showGrabbing():void{
	    this._color = 0x77EE77;
	    this.draw();
	}

	public function showReleased():void{
	    this._color = 0xFFFFFF;
	    this.draw();
	}

	
	
    }
}