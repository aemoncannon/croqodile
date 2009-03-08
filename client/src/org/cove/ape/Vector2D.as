/*
APE (Actionscript Physics Engine) is an AS3 open source 2D physics engine
Copyright 2006, Alec Cove 

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

Contact: ape@cove.org
*/
package org.cove.ape {
	
	import flash.errors.IllegalOperationError;
	
	public class Vector2D {
		
		public var x:Number;
		public var y:Number;
	
	
		public function Vector2D(px:Number, py:Number) {
			x = px;
			y = py;
		}
		
		
		public function setTo(px:Number, py:Number):void {
			x = px;
			y = py;
		}
		
		
		public function copy(v:Vector2D):void {
			x = v.x;
			y = v.y;
		}
	
	
		public function dot(v:Vector2D):Number {
			return x * v.x + y * v.y;
		}
		
		
		public function cross(v:Vector2D):Number {
			return x * v.y - y * v.x;
		}
		
	
		public function plus(v:Vector2D):Vector2D {
			return new Vector2D(x + v.x, y + v.y); 
		}
	
		
		public function plusEquals(v:Vector2D):Vector2D {
			x += v.x;
			y += v.y;
			return this;
		}
		
		
		public function minus(v:Vector2D):Vector2D {
			return new Vector2D(x - v.x, y - v.y);    
		}
	
	
		public function minusEquals(v:Vector2D):Vector2D {
			x -= v.x;
			y -= v.y;
			return this;
		}
	
	
		public function mult(s:Number):Vector2D {
			return new Vector2D(x * s, y * s);
		}
	
	
		public function multEquals(s:Number):Vector2D {
			x *= s;
			y *= s;
			return this;
		}
	
	
		public function times(v:Vector2D):Vector2D {
			return new Vector2D(x * v.x, y * v.y);
		}
		
		
		public function divEquals(s:Number):Vector2D {
			if (s == 0) s = 0.0001;
			x /= s;
			y /= s;
			return this;
		}
		
		
		public function magnitude():Number {
			return Math.sqrt(x * x + y * y);
		}

		
		public function distance(v:Vector2D):Number {
			var delta:Vector2D = this.minus(v);
			return delta.magnitude();
		}

	
		public function normalize():Vector2D {
			 var m:Number = magnitude();
			 if (m == 0) m = 0.0001;
			 return mult(1 / m);
		}
		
				
		public function toString():String {
			return (x + " : " + y);
		}
	}
}