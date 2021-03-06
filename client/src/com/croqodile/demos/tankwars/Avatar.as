package com.croqodile.demos.tankwars {
    import flash.display.*;
    import flash.events.*;
    import flash.utils.*;
    import com.croqodile.*;
    import com.croqodile.demos.tankwars.*;
    import org.cove.ape.*;
    import flash.ui.Keyboard;
    
    public class Avatar extends PhysObj {
		
		private var _userId:String;
		private var _wordBubbleFadeAnimator:Animator;
		private var _wordBubble:WordBubbleView;
		private var _rotation:Number = 0;
		
		public function Avatar(island:IslandReplica, userId:String = null){
			var part:CircleParticle = new CircleParticle(
				100, //x
				100, //y
				25, //radius
				false, //fixed?
				1, //mass
				0.3, //elasticity
				0.2  //friction
			);
			super(island, part);

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

		override public function get typeId():String{ return "Avatar"; }
		
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
			b.writeDouble(_rotation);
			b.writeUTF(_wordBubble.text),
			b.writeDouble(_wordBubble.alpha),
			_wordBubbleFadeAnimator.writeTo(b);
		}
		
		override public function readFrom(b:IDataInput):void{
			super.readFrom(b)
			_userId = b.readUTF();
			_rotation = b.readDouble();
			_wordBubble.text = b.readUTF();
			_wordBubble.alpha = b.readDouble();
			_wordBubbleFadeAnimator.readFrom(b);
			render();
		}

		public static function readFrom(b:IDataInput, island:IslandReplica):Avatar{
			var avatar:Avatar = new Avatar(island);
			avatar.readFrom(b);
			return avatar;
		}

		protected function rotate(d:Number):void{
			_rotation += d;
			_view.rotation = _rotation; 
		}

		protected function fireBullet():void{
			
		}


		override public function step():void{
			rotate(_rotationRate);
			var r:Number = (_rotation * Math.PI) / 180.0 - (Math.PI / 2.0);
			_particle.addForce(new Vector2D(_thrust * Math.cos(r), _thrust * Math.sin(r)));
		}

		
		///////////////////////////////
        // External Interface	     //
        ///////////////////////////////
		
		
		public function sayWords(words:String):void{
			_wordBubbleFadeAnimator.restart();
			_wordBubble.text = "\"" + words + "\"";
		}


		private var _rotationRate:Number = 0;
		private var _thrust:Number = 0;

		public function keyDown(code:int):void{
			switch(code){
				case Keyboard.LEFT:
				_rotationRate = -5;
				break;

				case Keyboard.RIGHT:
				_rotationRate = 5;
				break;

				case Keyboard.UP:
				_thrust = 2;
				break;

				case Keyboard.DOWN:
				_thrust = -2;
				break;

				case Keyboard.SPACE:
				fireBullet();
				break;
			}
		}


		public function keyUp(code:int):void{
			switch(code){
				case Keyboard.LEFT:
				_rotationRate = 0;
				break;

				case Keyboard.RIGHT:
				_rotationRate = 0;
				break;

				case Keyboard.UP:
				_thrust = 0;
				break;

				case Keyboard.DOWN:
				_thrust = 0;
				break;
			}
		}


		
    }
}



import flash.display.*;
import flash.events.*;
import flash.text.*;



class AvatarView extends Sprite {
	
	public function AvatarView(){
		var h:Number = 40; /* lengthwise height of ship */
		var n:Number = 30; /* lengthwise height of ship without wingtips */
		var w:Number = 40; /* width wingtip to wingtip*/

		var g:Graphics = this.graphics;
		g.lineStyle(2.0, 0xffffff, 1.0);
		g.beginFill(0x000000, 0.0);
		g.moveTo(0, -h/2);
		g.lineTo(w/2, h/2);
		g.moveTo(0, -h/2);
		g.lineTo(-w/2, h/2);
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
		_field.text = val;
	}

	
}


