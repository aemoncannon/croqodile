package com.croqodile.util{
    import flash.utils.Timer;
    import com.croqodile.*;
    import flash.events.*;
    
    /*
     *  Based largely on Noel Billig's SeedRandom class (http://www.dncompute.com)
     */    
    public class SeededRandom {
	
	private var _seed:Number;
	
	function SeededRandom(seed:Number = 0) {
	    this._seed = seed;
	}
	
	public function freeze():Object { 
	    return { seed: this._seed }
	}
	
	public function unfreeze(data:Object):void {
	    this._seed = data.seed;
	}

	/**
	 *	Returns a pseudo-random number n, where 0 <= n < 1
	 */
	public function random():Number {
	    this._seed = (this._seed * 9301 + 49297) % 233280;
	    return this._seed / (233280.0);
	}
	
	
	/**
	 *	Utility method for getting real numbers in the provided range
	 *	The range is inclusive	 
	 */
	public function numInRange(bottom:Number, top:Number):Number {
	    var dif:Number = top-bottom + 1;
	    var num:Number = this.random();
	    return bottom + (dif * num);
	}
	
	
	/**
	 *	Utility method for getting integers numbers in the provided range
	 *	The range is inclusive
	 */
	public function intInRange(bottom:Number,top:Number):Number {
	    var dif:Number = top - bottom + 1;
	    var num:Number = this.random();
	    return Math.floor( bottom + (dif * num) );
	}
	
	
	public function boolean():Boolean {
	    return this.random() < .5;
	}
	
    }
    
}


