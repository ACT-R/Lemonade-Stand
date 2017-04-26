/**
 **  ACT-R / LISP INTERACTIVE INTERFACE
 **  @author Derek Brown <derekbro@andrew>
 **/

  var pty = require('node-pty');
  var LemonadeGame = require('./game.js').game;

  /* Create Database of Log Output */
  lisp_output = new Meteor.Collection('lisp_output');

  /* Terminal Creation */
  play_game = function(model, iterations = 100, col = 80, row = 24){

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

    // Command Output Function
    term.on('data', Meteor.bindEnvironment(function(data){
        lisp_output.insert({"data" : data.toString(), "terminal_id" : terninal_id});
    }));

    // Load Model
    for (line of code) {
      if(term.writable){
        term.write(line);
      } else {
        Meteor.Error("Cannot write to shell.");
        lisp_output.insert({"message" : "TERMINAL_ERROR", "terminal_id" : terninal_id});
        break;
      }
    }

    // Setup Game
    var game = new LemonadeGame();
    var i = 0;
    var timeout_id = null;

    var play_game = function(play){
      if(i < iterations){

        // Validate
        if(play != null)
          game.nextDay(play);

        // Show Score and Run Model
        term.write(`(learn-stage ${game.getScore()})
                    (purchase-stage
                        '(${game.getWeather().getTemp()} ${game.getWeather.getCond()})
                        '(${game.getInventory().lemons} ${game.getInventory().sugar} ${game.getInventory().ice} ${game.getInventory().cups})
                     )`);

        // Set Timeout to catch Model Timeout
        timeout_id = Meteor.setTimeout(function(){
              lisp_output.insert({"message" : "MODEL_TIMEOUT", "terminal_id" : terninal_id});
              term.kill();
        }, 15000);

        // Increment Day Counter
        i++;
      }
    }

    // Run Game
    term.on('data', Meteor.bindEnvironment(function(data){

      // Validate Command String
      if((parse = data.match(/(\d), (\d), (\d), (\d)/gi)) != null) &&
         (moves = parse.splice(0, 1).length == 4){

        // Iterate over and validate responses from model
        for(var i = 0; i < 4; i++){
          if((moves[i] = parseInt(moves[i])) > 1 || moves[i] < 0){
            throw "Move must be either 0 or 1";
          } else {
            moves[i] = !!moves[i];
          }
        }

        // Cancel Timeout Checker
        if(timeout_id != null){
          Meteor.clearTimeout(timeout_id);
          timeout_id = null;
        }

        // Pass to Game Loop
        play_game(moves);
      }
    }));
    play_game(null);

    return terninal_id;
  }

  // Meteor Methods
  Meteor.methods({
    'play_game': play_game
  })
