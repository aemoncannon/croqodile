
package com.croqodile.serialization.json {

	/**
	 *
	 *
	 */
	public class JSONParseError extends Error 	{
	
		/** The location in the string where the error occurred */
		private var _location:int;
		
		/** The string in which the parse error occurred */
		private var _text:String;
	
		/**
		 * Constructs a new JSONParseError.
		 *
		 * @param message The error message that occured during parsing
		 * @langversion ActionScript 3.0
		 * @playerversion Flash 9.0
		 * @tiptext
		 */
		public function JSONParseError( message:String = "", location:int = 0, text:String = "") {
			super( message );
			//name = "JSONParseError";
			_location = location;
			_text = text;
		}

		/**
		 * Provides read-only access to the location variable.
		 *
		 * @return The location in the string where the error occurred
		 * @langversion ActionScript 3.0
		 * @playerversion Flash 9.0
		 * @tiptext
		 */
		public function get location():int {
			return _location;
		}
		
		/**
		 * Provides read-only access to the text variable.
		 *
		 * @return The string in which the error occurred
		 * @langversion ActionScript 3.0
		 * @playerversion Flash 9.0
		 * @tiptext
		 */
		public function get text():String {
			return _text;
		}
	}
	
}