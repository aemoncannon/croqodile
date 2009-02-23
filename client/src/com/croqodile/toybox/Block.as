package com.croqodile.toybox {
    import flash.display.MovieClip;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import com.croqodile.*;
    import com.croqodile.toybox.*;
    import org.cove.ape.*;
    
    public class Block extends IslandObject {
	
	private var _particle:BlockParticle;
	
	public function Block(island:IslandReplica, 
			      x:Number = 300,
			      y:Number = 200,
			      width:Number = 50, 
			      height:Number = 50, 
			      rotation:Number = 0,
			      mass:Number = 5.0,
			      friction:Number = 0.8){
	    super(island);
	    var part:BlockParticle = new BlockParticle(
						       x, //x
						       y, //y
						       width, //width
						       height, //height
						       rotation, //rotation
						       false, //fixed?
						       mass, //mass
						       0.3, //elasticity
						       friction); //friction
	    APEngine.addParticle(part);
	    this._particle = part;
	}
	
	public function addForce(x:Number, y:Number):void{
	    this._particle.addForce(new Vector(x,y));
	}
	
	public function render():void{
	    BlockParticle(this._particle).paint();
	}
	
	public function freeze():Object{
	    var data:Object = {
		collidable: this._particle.collidable,
		fixed: this._particle.fixed,
		friction: this._particle.friction,
		mass: this._particle.mass,
		positionX: this._particle.position.x,
		positionY: this._particle.position.y,
		px: this._particle.px,
		py: this._particle.py,
		width: this._particle.width,
		height: this._particle.height,
		rotation: this._particle.rotation,
		velX: this._particle.velocity.x,
		velY: this._particle.velocity.y,
		visible: this._particle.visible
	    }
	    return data;
	}
	
	public function unfreeze(data:Object):void{
	    this._particle.collidable = data.collidable;
	    this._particle.fixed = data.fixed;
	    this._particle.friction = data.friction;
	    this._particle.mass = data.mass;
	    this._particle.position = new Vector(data.positionX, data.positionY);
	    this._particle.px = data.px;
	    this._particle.py = data.py;
	    this._particle.width = data.width;
	    this._particle.height = data.height;
	    this._particle.rotation = data.rotation;
	    this._particle.velocity = new Vector(data.velX, data.velY);
	    this._particle.visible = data.visible;
	}
	
    }
}

