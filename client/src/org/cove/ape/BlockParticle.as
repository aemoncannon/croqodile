package org.cove.ape {
    
    /**
     * A rectangular shaped particle. Derived from RectangleParticle, with modifications to ease 
     * snapshotting of state.
     */ 
    public class BlockParticle extends RectangleParticle {
	
	
	protected var _width:Number;
	protected var _height:Number;

	public function BlockParticle (
				       x:Number, 
				       y:Number, 
				       width:Number, 
				       height:Number,
				       rotation:Number,
				       fixed:Boolean,
				       mass:Number = 1, 
				       elasticity:Number = 0.3,
				       friction:Number = 0) {
	    
	    super(x, y, width, height, rotation, fixed, mass, elasticity, friction);
	    this.width = width;
	    this.height = height;
	}
	

	public function get width():Number {
	    return _width;
	}
	public function set width(t:Number):void {
	    _width = t;
	    _extents = new Array(_width/2, _height/2);
	}
	
	public function get height():Number {
	    return _height;
	}
	public function set height(t:Number):void {
	    _height = t;
	    _extents = new Array(_width/2, _height/2);
	}
	
    }
}