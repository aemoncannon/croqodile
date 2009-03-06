package com.croqodile.demos.tankwars {
    import flash.display.*;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import flash.text.TextField;
    import flash.text.TextFormat;
    
    public class WordBubbleClip extends Sprite {
	public var _field:TextField;
	
	[Embed(source="C:\\WINDOWS\\Fonts\\ARIAL.TTF", fontFamily="Arial")]
	    private var _arialStr:String;
	
	public function WordBubbleClip(){
	    
	    // Set up a background shape for the text ..
	    var rect:Shape = new Shape();
	    rect.graphics.beginFill(0xFFFFFF);
	    rect.graphics.drawRect(0, 0, 100, 100);
	    rect.x = 0;
	    rect.y = 0;
	    rect.alpha = 0.0; //make it invisible
	    this.addChild(rect);
	    
	    var arialFmt:TextFormat = new TextFormat();
	    arialFmt.font = "Arial";
	    arialFmt.size = 12;
	    
	    this._field = new TextField();
	    this._field.embedFonts = true;
	    this._field.defaultTextFormat = arialFmt;
	    this._field.backgroundColor = 0xFFFFFF;
	    this._field.multiline = true;
	    this._field.wordWrap = true;
	    this._field.x = 0;
	    this._field.y = 0;
	    this._field.width = this.width;
	    this._field.height = this.height;
	    this._field.text = "";
	    this.addChild(this._field);
	}
	
	public function setText(str:String):void{
	    this._field.text = str;
	}

	public function text():String{
	    return this._field.text;
	}

	
    }
}

