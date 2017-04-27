/**
 **  ACT-R / LISP INTERACTIVE INTERFACE
 **  @author Derek Brown <derekbro@andrew>
 **/

  // Global Imports
  var nexpect = require('nexpect');
  var tmp = require('tmp');
  var fs = require('fs');

  // Import Lemonade Game
  var LemonadeGame = require('./game.js').game;

  /* Create Database of Log Output */
  lisp_output = new Meteor.Collection('lisp_output');

  /**
   **  runModel
   **
   **/
  var runModel = function(model, iterations = 100){
    // Create Terminal Record
    terninal_id = lisp_output.insert({"message" : "MODEL_NEW"});

    // Create Model File
    tmp.file({ postfix : ".lisp", detachDescriptor : true, keep : true }, Meteor.bindEnvironment(function(err, path_model, fd, cleanup) {
      if (err) {
        lisp_output.insert({"message" : "FILE_ERROR"});
      }

      // Write Model to File
      fs.write(fd, model, Meteor.bindEnvironment(function(err){
        if (err) {
          lisp_output.insert({"message" : "FILE_ERROR"});
          console.err(err);
        }

        fs.close(fd, Meteor.bindEnvironment(function(err){
          if (err) {
            lisp_output.insert({"message" : "FILE_ERROR"});
            console.err(err);
          }

          // Create PTY
          const path_ccl = Assets.absoluteFilePath("bin/ccl/lx86cl64");
          fs.chmodSync(path_ccl, '755');
          const path_actr = Assets.absoluteFilePath("bin/actr7/load-act-r.lisp");
          fs.chmodSync(path_actr, '755');

          term = nexpect
            .spawn(path_ccl, ["-l",path_actr,"-l",path_model])
            .wait("######### Loading of ACT-R 7 is complete #########")
            .wait("?");

          // Run Game Simulation
          var game = new LemonadeGame();

          for(var i = 1; i <= iterations; i++){
            // Generate Lisp Command
            var lisp_command =
                    `(learn-stage ${game.getScore()})
                     (purchase-stage
                          '(${game.getWeather().getTemp()}
                            ${game.getWeather().getCond()}
                           )
                          '(${game.getInventory().lemons}
                            ${game.getInventory().sugar}
                            ${game.getInventory().ice}
                            ${game.getInventory().cups}
                           )
                    )`

            // Send Lisp Command
            term = term.sendline(lisp_command)
                        .wait(/"([0-1]), ([0-1]), ([0-1]), ([0-1])"/ig, Meteor.bindEnvironment(function(data){

                          // Format Model Output
                          var moves = [];
                          re = /"([0-1]), ([0-1]), ([0-1]), ([0-1])"/ig;
                          if((parse = re.exec(data)) != null){
                            for(var i = 0; i < 4; i++){
                                moves[i] = parse[i] == 1;
                            }

                            // Return Results of Model
                            var move_string = `
                            == DAY ${game.getDay()} ==
                            SCORE: ${game.getScore()}
                            WEATHER: ${game.getWeather().getTemp()} ${game.getWeather().getCond()}
                            INVENTORY: L: ${game.getInventory().lemons} S: ${game.getInventory().sugar} I: ${game.getInventory().ice} C: ${game.getInventory().cups}

                            MOVE: ${data}
                            `;
                            lisp_output.insert({"data" : move_string, "terminal_id" : terninal_id});

                            // Update Game State
                            game.nextDay(moves);
                          }
                        }));
          }

          // Close ACT-R
          term.sendline("(quit)")
              .run(Meteor.bindEnvironment(function(err, output, exit){
                if (!err) {
                  lisp_output.insert({"message" : "MODEL_SUCCESS", "terminal_id" : terninal_id});
                }
                else {
                  lisp_output.insert({"message" : "MODEL_FAILURE", "terminal_id" : terninal_id});
                }

                // Cleanup TMP File
                cleanup();
              }));

        }));
      }));
    }));

    return terninal_id;
  }

  // Meteor Methods
  Meteor.methods({
    'play_game': runModel
  })
