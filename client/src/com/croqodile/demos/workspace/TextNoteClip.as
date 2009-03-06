package com.croqodile.workspace {
    import flash.display.*;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import flash.text.TextField;
    import flash.text.TextFormat;
    
    public class TextNoteClip extends Sprite {
	private var _field:TextField;
	private var _bg:Shape;
	
	[Embed(source="C:\\WINDOWS\\Fonts\\ARIAL.TTF", fontFamily="Arial")]
	    private var _arialStr:String;
	
	public function TextNoteClip(x, y, width, height){
	    // Set up a background shape for the text ..
	    this._bg = new Shape();
	    this.addChild(this._bg);
	    
	    var arialFmt:TextFormat = new TextFormat();
	    arialFmt.font = "Arial";
	    arialFmt.size = 12;
	    
	    this._field = new TextField();
	    this._field.embedFonts = true;
	    this._field.defaultTextFormat = arialFmt;
	    this._field.backgroundColor = 0xFFFFFF;
	    this._field.multiline = true;
	    this._field.wordWrap = true;
	    this._field.text = "";
	    this.addChild(this._field);
	    
	    this.setDimensions(x, y, width, height);
	}
	
	private function draw():void{
	    this._bg.graphics.beginFill(0xFFFFFF);
	    this._bg.graphics.drawRect(0, 0, this._width, this._height);
	    this._bg.alpha = 0.0; //make it invisible
	}
	
	public function setText(str:String):void{
	    this._field.text = str;
	}
	
	public function text():String{
	    return this._field.text;
	}
	
	public function setDimensions(x:Number, y:Number, width:Number, height:Number):void{
	    this._width = width;
	    this._height = height;
	    this._field.x = x;
	    this._field.y = y;
	    this._field.width = this._width;
	    this._field.height = this._height;
	    this._bg.x = x;
	    this._bg.y = y;
	    this._bg.width = this._width;
	    this._bg.height = this._height;
	    this.draw();
	}
	
	
    }
}

