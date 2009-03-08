function init(){
	refreshDirectory();
	var link = $("createIslandLink");
	link.onclick = function(){
		new Ajax.Request(
			"/new_island", {
				method: 'get',
				parameters: { type: 'toybox', desc: "A fun game." },
				onSuccess: function(transport){
					refreshDirectory();
				},
				onFailure: function(){
					alert('Failed to create new island.');
				}
			});
		return true;
	};
}

function refreshDirectory(){
	new Ajax.Request(
		'/directory',
		{
			method: 'get',
			onSuccess: function(transport){
				var response = transport.responseText || "[]";
				var directory = response.evalJSON();
				var ul = $("directoryList");
				directory.each(function(ea){
								   var text = "id: " + ea.id + "  type: " + ea.type;
								   var li = new Element("li", {});
								   var url = ["/island.html?",
											  "islandId=", ea.id,
											  "&host=", "localhost",
											  "&port=", "6666",
											  "&policyPort=", "6665",
											  "&userId=", "aemon"
											 ].join("");
								   var link = new Element("a", {href: url}).update(text);
								   li.appendChild(link);
								   ul.appendChild(li);
							   });
			},
			onFailure: function(){
				alert('Failed to load directory from /directory');
			}
		});
}


init();
