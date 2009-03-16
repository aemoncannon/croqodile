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
	
	/**	
	 * A circle shaped particle. 	 
	 */
	public class CircleParticle extends AbstractParticle {
	
		private var _radius:Number;
		
		/**
		 * @param x The initial x position of this particle.
		 * @param y The initial y position of this particle.
		 * @param radius The radius of this particle.
		 * @param fixed Determines if the particle is fixed or not. Fixed particles
		 * are not affected by forces or collisions and are good to use as surfaces.
		 * Non-fixed particles move freely in response to collision and forces.
		 * @param mass The mass of the particle.
		 * @param elasticity The elasticity of the particle. Higher values mean more elasticity or 'bounciness'.
		 * @param friction The surface friction of the particle.
		 */
		public function CircleParticle (
				x:Number = 0, 
				y:Number = 0, 
				radius:Number = 10, 
				fixed:Boolean = false,
				mass:Number = 1, 
				elasticity:Number = 0.3,
				friction:Number = 0) {
					
			super(x, y, fixed, mass, elasticity, friction);
			_radius = radius;
		}

		/**
		 * The radius of the particle.
		 */
		public function get radius():Number {
			return _radius;
		}		
		
		
		/**
		 * @private
		 */
		public function set radius(r:Number):void {
			_radius = r;
		}
		
		
	
	
		// REVIEW FOR ANY POSSIBILITY OF PRECOMPUTING
		/**
		 * @private
		 */
		internal override function getProjection(axis:Vector2D):Interval {
			var c:Number = curr.dot(axis);
			interval.min = c - _radius;
			interval.max = c + _radius;
			return interval;
		}
		
		
		/**
		 * @private
		 */
		internal function getIntervalX():Interval {
			interval.min = curr.x - _radius;
			interval.max = curr.x + _radius;
			return interval;
		}
		
		
		/**
		 * @private
		 */		
		internal function getIntervalY():Interval {
			interval.min = curr.y - _radius;
			interval.max = curr.y + _radius;
			return interval;
		}
	}
}
	
	