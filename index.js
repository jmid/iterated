// Registered helper accumulates a list of 'lint'-like hints
CodeMirror.registerHelper("lint", "clike", function(text) {
    var result = document.getElementById('result');
    var resobj = parse(text);
    var found = [];
    if (resobj.error != undefined) {
	var line   = resobj.error.line - 1;
	var column = resobj.error.column - 1;
	found.push({ from:     CodeMirror.Pos(line, column),
		     to:       CodeMirror.Pos(line, column + 1),
		     message:  resobj.error.msg,
		     severity: "error"
		   });
    }
    return found;
});

var editor1 = CodeMirror.fromTextArea(document.getElementById("texteditor1"), {
    mode            : 'text/x-csrc',
    theme           : "eclipse",
    lineNumbers     : true,
    firstLineNumber : 0, 
    gutters         : ["CodeMirror-linenumbers"],
});

var editor2 = CodeMirror.fromTextArea(document.getElementById("texteditor2"), {
    mode            : 'text/x-csrc',
    theme           : "eclipse",
    lineNumbers     : true,
    firstLineNumber : 0, 
    gutters         : ["CodeMirror-linenumbers"],
});

function swap() {
    tmp = editor1.getValue();
    editor1.setValue(editor2.getValue());
    editor2.setValue(tmp);
}
var swapbutton = document.getElementById('swap');
swapbutton.addEventListener('click', swap);


function msg(obj) {
    return obj.msg + ': line ' + obj.line + ', column ' + obj.column;
}

function analyze() {
    var result = document.getElementById('result');
    var resobj = interpret(editor1.getValue(),editor2.getValue());
    if (resobj.error1 != undefined)
	result.innerText = msg(resobj.error1);
    else if (resobj.error2 != undefined)
	result.innerText = msg(resobj.error2);
    else {
	console.log(resobj.result);
	result.innerHTML = '<pre>' + resobj.result + '</pre>';
    }
}

var abutton = document.getElementById('analyze');
abutton.addEventListener('click', analyze);

var simpleprog = { p1 : "spawn proc1() { ch?x }",
	           p2 : "spawn proc2() { ch!42 }" };

var deadlockprog = { p1 : "spawn proc1() { ch_a?x;\n"
                        + "                ch_b!1; }",
	             p2 : "spawn proc2() { ch_b?y;\n"
                        + "                ch_a!2; }" };

var whileprog  = { p1 : "spawn proc1() { while true { ch?x } }",
	           p2 : "spawn proc2() { x = 0;\n"
  		      + "                while true { ch!x;\n"
                      + "                             x = x-1; } }"   };

var whileextprog  = { p1 : "spawn proc1() { while (0 < x) { ch?x } }",
	              p2 : "spawn proc2() { x = 0;\n"
  	  	         + "                while true { ch!x;\n"
                         + "                             x = x-1; } }"   };

var nontermination = { p1 : "spawn proc1() { x = 1;\n"
		          + "                while (0 < x) { ch?x } }",
	               p2 : "spawn proc2() { y = 1000;\n"
  	  	          + "                while (0 < y) { ch!y;\n"
                          + "                                y = y-1; } }"   };

function progSelect() {
    var pselect = document.getElementById('prog-select');
    var choice = pselect.value;
    if (choice == 'simple')        { editor1.setValue(simpleprog.p1);
			  	     editor2.setValue(simpleprog.p2); }
    else if (choice == 'deadlock') { editor1.setValue(deadlockprog.p1);
				     editor2.setValue(deadlockprog.p2); }
    else if (choice == 'while')    { editor1.setValue(whileprog.p1);
				     editor2.setValue(whileprog.p2); }
    else if (choice == 'whileext') { editor1.setValue(whileextprog.p1);
				     editor2.setValue(whileextprog.p2); }
    else                           { editor1.setValue(nontermination.p1);
				     editor2.setValue(nontermination.p2); }
    console.log("Selected value ", pselect.value);
};


var pselect = document.getElementById('prog-select');
pselect.addEventListener('change', progSelect);
