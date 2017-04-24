/**
 **  ACT-R / LISP INTERACTIVE INTERFACE
 **  @author Derek Brown <derekbro@andrew>
 **/

  var pty = require('node-pty');

  /* Create Database of Log Output */
  lisp_output = new Meteor.Collection('lisp_output');

  /* Terminal Creation */
  execute_model = function(code, col = 80, row = 24){

    // Verify Terminal Size
    if (col > 180)
      col = 180;
    if (col < 80)
      col = 80;
    if (row > 100)
      row = 100;
    if (row < 24)
      row = 24;

    // Create Terminal Record
    terninal_id = lisp_output.insert({"message" : "NEW_TERMINAL"});

    // Create PTY
    command = "./private/bin/ccl/lx86cl64";
    actr = "./private/bin/actr7/load-act-r.lisp";

    term = pty.spawn(command, [], {
      name: 'xterm-color',
      cols: col,
      rows: row,
      cwd: process.env.PWD,
      env: process.env
    });

    // Forward Data to Log
    term.on('data', Meteor.bindEnvironment(function(data){
        lisp_output.insert({"data" : data.toString(), "terminal_id" : terninal_id});
    }));

    // Pipe Data into Command
    for (line of code) {
      if(term.writable){
        term.write(line);
      } else {
        Meteor.Error("Cannot write to shell.");
        lisp_output.insert({"message" : "TERMINAL_ERROR", "terminal_id" : terninal_id});
        break;
      }
    }

    // Wait for 15s Timeout and Close
    Meteor.setTimeout(function(){
      lisp_output.insert({"message" : "MODEL_TIMEOUT", "terminal_id" : terninal_id});
      term.kill();
    }, 15000);

    return terninal_id;
  }

  // Meteor Methods
  Meteor.methods({
    'execute_model': execute_model
  })
