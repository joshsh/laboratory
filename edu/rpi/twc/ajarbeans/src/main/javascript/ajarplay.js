
// var kb = new RDFFormula()    // Using this gives no smushing
var kbase = new RDFIndexedFormula();  // This uses indexing and smushing
var fetcher = new SourceFetcher(kbase); // This handles resource retrieval
kbase.register('dc', "http://purl.org/dc/elements/1.1/");
kbase.register('rdf', "http://www.w3.org/1999/02/22-rdf-syntax-ns#");
kbase.register('rdfs', "http://www.w3.org/2000/01/rdf-schema#");
kbase.register('owl', "http://www.w3.org/2002/07/owl#");
kbase.register('foaf', "http://xmlns.com/foaf/0.1/");
kbase.predicateCallback = AJAR_handleNewTerm;
kbase.typeCallback = AJAR_handleNewTerm;


function outline_expand(subject) {
    // This is an artifact of outline_expand
    var p = null;

    var subjuri = subject.uri?kbase.sym(Util.uri.docpart(subject.uri)):subject
    var sources = false

    function expand(uri, requestedBy) {
	var udoc = uri.uri?kbase.sym(Util.uri.docpart(uri.uri)):uri
	if ((uri.sameTerm(subject)
	     || kbase.anyStatementMatching(udoc,kbase.sym('tab',"requestedBy"),
					subjuri))
	    && !sources[udoc.toString()]) { // for refreshes
	    if (!p || !p.parentNode || !p.parentNode.parentNode) return false
	    var newTable
	    if (!sources) { // new expand
		sources = {}
		sources[udoc.toString()] = true
    		newTable = propertyTable(subject)
	    } else {
		sources[udoc.toString()] = true
		for (newTable = p.firstChild; newTable.nextSibling;
		     newTable = newTable.nextSibling) {
		    if (newTable.nodeName == 'table') break
		}
		newTable = propertyTable(subject, newTable)
	    }
    	    if (p.parentNode.parentNode.style.backgroundColor=='white') {
    		newTable.style.backgroundColor='#eee'
	    } else {
		newTable.style.backgroundColor='white'
	    }
    	    emptyNode(p).appendChild(newTable)
	    log.debug("expand: Node for " + subject + " expanded")
	}
	return true
    }

    log.debug("outline_expand: dereferencing "+subject)
    var status = document.createElement("span")
    p.appendChild(status)
    fetcher.addCallback('done', expand)
    fetcher.addCallback('fail', expand)
    fetcher.addCallback('request', function (u) {
		       if (!u.sameTerm(subject)) { return true }
		       status.textContent=" requested..."
		       return false
		   })
    fetcher.addCallback('recv', function (u) {
		       if (!u.sameTerm(subject)) { return true }
		       status.textContent=" receiving..."
		       return false
		   })
    fetcher.addCallback('load', function (u) {
		       if (!u.sameTerm(subject)) { return true }
		       status.textContent=" parsing..."
		       return false
		   })
    fetcher.request(subject)

} //outline_expand

