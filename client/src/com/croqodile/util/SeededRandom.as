package com.croqodile.util{
    import flash.utils.Timer;
    import com.croqodile.*;
    import flash.events.*;
    import flash.utils.*;
    
    /*
    *  Based largely on Noel Billig's SeedRandom class (http://www.dncompute.com)
    */    
    public class SeededRandom {
		
		private var _seed:Number;
		
		function SeededRandom(seed:Number = 0) {
			_seed = seed;
		}
		
		public function readFrom(b:IDataInput):void { 
			_seed = b.readDouble();
		}
		
		public function writeTo(b:IDataOutput):void {
			b.writeDouble(_seed);
		}

		/**
		*	Returns a pseudo-random number n, where 0 <= n < 1
		*/
		public function random():Number {
			_seed = (_seed * 9301 + 49297) % 233280;
			return _seed / (233280.0);
		}
		
		
		/**
		*	Utility method for getting a double in the provided, inclusive range.
		*
		*/
		public function numInRange(bottom:Number, top:Number):Number {
			var dif:Number = top-bottom + 1;
			var num:Number = random();
			return bottom + (dif * num);
		}
		
		
		/**
		*	Utility method for getting integers numbers in the provided range
		*	The range is inclusive
		*/
		public function intInRange(bottom:Number,top:Number):Number {
			var dif:Number = top - bottom + 1;
			var num:Number = random();
			return Math.floor( bottom + (dif * num) );
		}
		
		
		public function boolean():Boolean {
			return random() < .5;
		}
		
    }
    
}


