package com.croqodile.events {
    import flash.events.Event;
    
    public class SnapshotReceivedEvent extends Event {
	
	public static var type:String = "snapshotReceived";
	public var snapshot:String;

	public function SnapshotReceivedEvent(snapshot:String){
	    super(SnapshotReceivedEvent.type);
	    this.snapshot = snapshot;
	}
	
    }
}

