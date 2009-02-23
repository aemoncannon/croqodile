package com.croqodile.toybox {
    import flash.display.MovieClip;
    import flash.display.Sprite;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import com.croqodile.*;
    import com.croqodile.toybox.*;
    import org.cove.ape.*;
    
    public class Avatar extends IslandObject {
		
		private var _userId:String;
		private var _clip:MovieClip;
		private var _particle:AbstractParticle;
		private var _wordBubbleFadeAnimator:Animator;
		private var _wordBubble:WordBubbleClip;
		
		public function Avatar(island:IslandReplica, userId:String){
			super(island);
			_userId = userId;
			_clip = new AvatarClip();
			var canvas:Sprite = ToyboxIsland(island).canvas();
			canvas.addChild(_clip);
			var part:CircleParticle = new CircleParticle(100, //x
				100, //y
				25, //radius
				false, //fixed?
				1, //mass
				0.3, //elasticity
				0.2); //friction
			APEngine.addParticle(part);
			_particle = part;

			_wordBubble = new WordBubbleClip();
			canvas.addChild(_wordBubble);
			
			var self:Avatar = this;

			_wordBubbleFadeAnimator = new Animator(island, 400, function(step:int):Boolean {
					self._wordBubble.alpha = (1 - (step/10.0));
					return step < 10;
				});
		}
		
		public function render():void{
			_clip.x = _particle.px;
			_clip.y = _particle.py;

			_wordBubble.x = _clip.x + _clip.width;
			_wordBubble.y = _clip.y - 20;
		}
		
		public function freeze():Object{
			var data:Object = {
				userId: _userId,
				collidable: _particle.collidable,
				fixed: _particle.fixed,
				friction: _particle.friction,
				mass: _particle.mass,
				positionX: _particle.position.x,
				positionY: _particle.position.y,
				px: _particle.px,
				py: _particle.py,
				velX: _particle.velocity.x,
				velY: _particle.velocity.y,
				visible: _particle.visible,
				
				wordBubbleText: _wordBubble.text(),
				wordBubbleAlpha: _wordBubble.alpha,

				wordBubbleFadeAnimator: _wordBubbleFadeAnimator.freeze()
			}
			return data;
		}
		
		public function unfreeze(data:Object):void{
			_userId = data.userId;
			_particle.collidable = data.collidable;
			_particle.fixed = data.fixed;
			_particle.friction = data.friction;
			_particle.mass = data.mass;
			_particle.position = new Vector(data.positionX, data.positionY);
			_particle.px = data.px;
			_particle.py = data.py;
			_particle.velocity = new Vector(data.velX, data.velY);
			_particle.visible = data.visible;

			_wordBubble.setText(data.wordBubbleText);
			_wordBubble.alpha = data.wordBubbleAlpha;
			
			_wordBubbleFadeAnimator.unfreeze(data.wordBubbleFadeAnimator);
		}
		
		
		///////////////////////////////
        // External Interface	     //
        ///////////////////////////////
		
		
		public function addForce(x:Number, y:Number):void{
			_particle.addForce(new Vector(x,y));
		}

		public function sayWords(words:String):void{
			_wordBubbleFadeAnimator.restart();
			_wordBubble.setText("\"" + words + "\"");
		}
		
		
    }
}
