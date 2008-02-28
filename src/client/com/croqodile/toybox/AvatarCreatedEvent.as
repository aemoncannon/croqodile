package com.croqodile.toybox {
    import flash.events.Event;
    import com.croqodile.FarRef;
    
	public class AvatarCreatedEvent extends Event {
	    public var userId:String;
	    public var avatarRef:FarRef;
	    public static var type:String = "avatarCreated";
	    
	    public function AvatarCreatedEvent( avatarRef:FarRef, userId:String){
		super(AvatarCreatedEvent.type);
		this.userId = userId;
		this.avatarRef = avatarRef;
	    }
	    
	}
}

