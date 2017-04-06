/**
 **  ACT-R / LISP INTERACTIVE INTERFACE
 **  @author Derek Brown <derekbro@andrew>
 **/

  var pty = require('node-pty');

  /* Create Database of Log Output */
  lisp_output = new Meteor.Collection('lisp_output', {connection: null});

  /* Terminal Creation */
  execute_model = function(code, col = 80, row = 24){

    // Delete All in Collection
    lisp_output.remove({});

    // Verify Terminal Size
    if (col > 180)
      col = 180;
    if (col < 80)
      col = 80;
    if (row > 100)
      row = 100;
    if (row < 24)
      row = 24;

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
        lisp_output.insert({"data" : data.toString()});
    }));

    // Pipe Data into Command
    for (line of code) {
      if(term.writable){
        term.write(line);
      } else {
        Meteor.Error("Cannot write to shell.");
        break;
      }
    }

    // Wait for 15s Timeout and Close
    Meteor.setTimeout(function(){
      term.kill();
    }, 15000);

    return;
  }

  // Meteor Methods
  Meteor.methods({
    'execute_model': execute_model
  })
