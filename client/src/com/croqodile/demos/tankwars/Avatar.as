package com.croqodile.demos.tankwars {
    import flash.display.*;
    import flash.events.*;
    import flash.utils.*;
    import com.croqodile.*;
    import com.croqodile.demos.tankwars.*;
    import org.cove.ape.*;
    
    public class Avatar extends PhysObj {
		
		private var _userId:String;
		private var _wordBubbleFadeAnimator:Animator;
		private var _wordBubble:WordBubbleView;
		
		public function Avatar(island:IslandReplica, userId:String = null){
			super(island);
			_userId = userId;

			_view = new AvatarView();
			var canvas:Sprite = TankWarsIsland(island).canvas;
			canvas.addChild(_view);

			_wordBubble = new WordBubbleView();
			canvas.addChild(_wordBubble);
			
			_wordBubbleFadeAnimator = new Animator(island, 400, function(step:int):Boolean {
					_wordBubble.alpha = (1 - (step/10.0));
					return step < 10;
				});
		}
		
		override public function render():void{
			if(_particle.px != _view.x || _particle.py != _view.y){
				_view.x = _particle.px;
				_view.y = _particle.py;
			}
			_wordBubble.x = _view.x + _view.width/2;
			_wordBubble.y = _view.y - 20;
		}


		override public function writeTo(b:IDataOutput):void{
			super.writeTo(b);
			b.writeUTF(_userId);
			b.writeUTF(_wordBubble.text),
			b.writeDouble(_wordBubble.alpha),
			_wordBubbleFadeAnimator.writeTo(b);
		}
		
		override public function readFrom(b:IDataInput):void{
			_particle = new CircleParticle();
			super.readFrom(b)
			_userId = b.readUTF();
			_wordBubble.text = b.readUTF();
			_wordBubble.alpha = b.readDouble();
			_wordBubbleFadeAnimator.readFrom(b);
			init();
		}

		public static function readFrom(b:IDataInput, island:IslandReplica):Avatar{
			var avatar:Avatar = new Avatar(island);
			avatar.readFrom(b);
			return avatar;
		}


		public static function createRandom(island:IslandReplica, userId:String, x0:Number, y0:Number, w:Number, h:Number):Avatar{
			var b:ByteArray = new ByteArray();
			writeBytesForCreateRandom(b, island, userId, x0, y0, w, h);
			return readFrom(b, island);
		}
		protected static function writeBytesForCreateRandom(b:ByteArray, island:IslandReplica, userId:String, x0:Number, y0:Number, w:Number, h:Number):void{
			PhysObj.writeBytesForCreateRandom(b, island, x0, y0, w, h);
			b.writeUTF(userId);
			b.writeUTF(""),
			b.writeDouble(0.0),
			_wordBubbleFadeAnimator.writeTo(b);
		}

		
		///////////////////////////////
        // External Interface	     //
        ///////////////////////////////
		
		
		public function sayWords(words:String):void{
			_wordBubbleFadeAnimator.restart();
			_wordBubble.text = "\"" + words + "\"";
		}
		
		
    }
}



import flash.display.*;
import flash.events.*;
import flash.text.*;



class AvatarView extends Sprite {
	
	public function AvatarView(){
		var g:Graphics = this.graphics;
		g.lineStyle(2.0, 0xff0000, 1.0);
		g.beginFill(0x000000, 0.0);
		g.drawCircle(0, 0, 25);
		g.endFill();
	}
	
}


class WordBubbleView extends Sprite {
	protected var _field:TextField;
	
	[Embed(source="Aller_Rg.ttf", fontFamily="Aller")]
	private var _arialStr:String;
	

	public function WordBubbleView(){

		// Set up a background shape for the text ..
		var rect:Shape = new Shape();
		rect.graphics.beginFill(0xFFFFFF);
		rect.graphics.drawRect(0, 0, 100, 100);
		rect.x = 0;
		rect.y = 0;
		rect.alpha = 0.0; //make it invisible
		addChild(rect);
		
		var arialFmt:TextFormat = new TextFormat();
		arialFmt.font = "Aller";
		arialFmt.size = 12;
		
		_field = new TextField();
		_field.embedFonts = true;
		_field.defaultTextFormat = arialFmt;
		_field.backgroundColor = 0xFFFFFF;
		_field.multiline = true;
		_field.wordWrap = true;
		_field.x = 0;
		_field.y = 0;
		_field.width = width;
		_field.height = height;
		_field.text = "";
		addChild(_field);
	}
	

	public function get text():String{
		return _field.text;
	}

	public function set text(val:String):void{
pp		_field.text = val;
	}

	
}


