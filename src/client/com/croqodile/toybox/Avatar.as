package com.croqodile.toybox {
    import flash.display.MovieClip;
    import flash.display.Sprite;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import com.senocular.utils.Output;
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
	    this._userId = userId;
	    this._clip = new AvatarClip();
	    var canvas:Sprite = ToyboxIsland(island).canvas();
	    canvas.addChild(this._clip);
	    var part:CircleParticle = new CircleParticle(100, //x
							 100, //y
							 25, //radius
							 false, //fixed?
							 1, //mass
							 0.3, //elasticity
							 0.2); //friction
	    APEngine.addParticle(part);
	    this._particle = part;

	    this._wordBubble = new WordBubbleClip();
	    canvas.addChild(this._wordBubble);
	    
	    var self:Avatar = this;

	    this._wordBubbleFadeAnimator = new Animator(island, 400, function(step:int):Boolean {
		    self._wordBubble.alpha = (1 - (step/10.0));
		    return step < 10;
		});
	}
	
	public function render():void{
	    this._clip.x = this._particle.px;
	    this._clip.y = this._particle.py;

	    this._wordBubble.x = this._clip.x + this._clip.width;
	    this._wordBubble.y = this._clip.y - 20;
	}
	
	public function freeze():Object{
	    var data:Object = {
		userId: this._userId,
		collidable: this._particle.collidable,
		fixed: this._particle.fixed,
		friction: this._particle.friction,
		mass: this._particle.mass,
		positionX: this._particle.position.x,
		positionY: this._particle.position.y,
		px: this._particle.px,
		py: this._particle.py,
		velX: this._particle.velocity.x,
		velY: this._particle.velocity.y,
		visible: this._particle.visible,
		
		wordBubbleText: this._wordBubble.text(),
		wordBubbleAlpha: this._wordBubble.alpha,

		wordBubbleFadeAnimator: this._wordBubbleFadeAnimator.freeze()
	    }
	    return data;
	}
	
	public function unfreeze(data:Object):void{
	    this._userId = data.userId;
	    this._particle.collidable = data.collidable;
	    this._particle.fixed = data.fixed;
	    this._particle.friction = data.friction;
	    this._particle.mass = data.mass;
	    this._particle.position = new Vector(data.positionX, data.positionY);
	    this._particle.px = data.px;
	    this._particle.py = data.py;
	    this._particle.velocity = new Vector(data.velX, data.velY);
	    this._particle.visible = data.visible;

	    this._wordBubble.setText(data.wordBubbleText);
	    this._wordBubble.alpha = data.wordBubbleAlpha;
	    
	    this._wordBubbleFadeAnimator.unfreeze(data.wordBubbleFadeAnimator);
	}
	
	
	///////////////////////////////
        // External Interface	     //
        ///////////////////////////////
	
	
	public function addForce(x:Number, y:Number):void{
	    this._particle.addForce(new Vector(x,y));
	}

	public function sayWords(words:String):void{
	    this._wordBubbleFadeAnimator.restart();
	    this._wordBubble.setText("\"" + words + "\"");
	}
	
	
    }
}
