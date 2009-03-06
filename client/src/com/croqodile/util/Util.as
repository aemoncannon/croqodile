package com.croqodile.util{

    import com.croqodile.Obj;
    import flash.utils.*;

    public class Util {

		static public function equal(k1:Object, k2:Object):Boolean{
			if(k1 === k2)
			return true;
			if(k1 === null || k1 is Number || k1 is Boolean || k1 is String)
			return false;
			if(k1 is Array){
				return arrayEqual(k1 as Array, k2);
			}
			if(k1 is ByteArray){
				return byteArrayEqual(ByteArray(k1), k2);
			}
			if(k1 is Obj){
				return k1.equals(k2);
			}
			return false;
		}


		static protected function arrayEqual(k1:Array, k2:Object):Boolean{
			if(!(k2 is Array) || k1.length != k2.length) return false
			for(var i:int = 0; i < k1.length; i++){
				if(k1[i] !== k2[i]) return false;
			}
			return true;
		}


		static protected function byteArrayEqual(k1:ByteArray, k2:Object):Boolean{
			if(!(k2 is ByteArray) || k1.length != k2.length) return false
			for(var i:int = 0; i < k1.length; i++){
				if(k1[i] !== k2[i]) return false;
			}
			return true;
		}

	}
}