package com.croqodile.toybox {
    import flash.display.MovieClip;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import com.senocular.utils.Output;
    import com.croqodile.*;
    import com.croqodile.toybox.*;
    import org.cove.ape.*;
    
    public class Thing extends IslandObject {
	
	private var _particle:AbstractParticle;
	
	public function Thing(island:IslandReplica){
	    super(island);
	    var part:CircleParticle = new CircleParticle(
							       300, //x
							       200, //y
							       10, //radius
							       false, //fixed?
							       0.2, //mass
							       0.3, //elasticity
							       0.1); //friction
	    APEngine.addParticle(part);
	    this._particle = part;
	}
	
	public function addForce(x:Number, y:Number):void{
	    this._particle.addForce(new Vector(x,y));
	}
	
	public function render():void{
	    CircleParticle(this._particle).paint();
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
	    this._particle.velocity = new Vector(data.velX, data.velY);
	    this._particle.visible = data.visible;
	}
	
    }
}

