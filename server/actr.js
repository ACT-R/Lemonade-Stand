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

  /**
   **  runModel
   **
   **/
  var runModel = function(obj){
    // Parse Obj
    model = obj.model;
    iterations = (obj.iterations != null) ? obj.iterations : 100;

    // Create Terminal Record
     terminal_id = lisp_output.insert({"message" : "MODEL_NEW"});

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
          const timeout = 20;
          const timeout_cmd = "/bin/timeout";
          const path_ccl = Assets.absoluteFilePath("bin/ccl/lx86cl64");
          fs.chmodSync(path_ccl, '755');
          const path_actr = Assets.absoluteFilePath("bin/actr7/load-act-r.lisp");
          fs.chmodSync(path_actr, '755');

          term = nexpect
            .spawn(timeout_cmd, ["--signal=SIGINT",timeout,path_ccl,"-l",path_actr,"-l",path_model])
            .wait("######### Loading of ACT-R 7 is complete #########")
            .expect("Welcome to Clozure Common Lisp Version 1.11-r16635  (LinuxX8664)!")
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
                        .expect(/"([0-1]), ([0-1]), ([0-1]), ([0-1])"/ig, Meteor.bindEnvironment(function(data){

                          // Format Model Output
                          var moves = [];
                          re = /"([0-1]), ([0-1]), ([0-1]), ([0-1])"/ig;
                          if((parse = re.exec(data)) != null){
                            for(var i = 0; i < 4; i++){
                                moves[i] = parse[i + 1] == 1;
                            }

                            // Return Results of Model
                            var move_string = `
                            == DAY ${game.getDay()} ==
                            SCORE: ${game.getScore()}
                            WEATHER: ${game.getWeather().getTemp()} ${game.getWeather().getCond()}
                            INVENTORY: L: ${game.getInventory().lemons} S: ${game.getInventory().sugar} I: ${game.getInventory().ice} C: ${game.getInventory().cups}

                            MOVE: ${data}
                            `;
                            lisp_output.insert({"data" : move_string, "terminal_id" :  terminal_id});

                            // Update Game State
                            game.nextDay(moves);
                          }
                        }));
          }

          // Close ACT-R
          term.sendline("(quit)")
              .run(Meteor.bindEnvironment(function(err, output, exit){

                // Success Case
                if(exit ==  0){
                  // Return Model Message
                  if (!err) {
                    lisp_output.insert({"message" : "MODEL_SUCCESS", "terminal_id" :  terminal_id});
                  }
                  else {
                    lisp_output.insert({"message" : "MODEL_FAILURE", "terminal_id" :  terminal_id});
                  }

                  // Return Model Data
                  data_output.insert({"data" : game.getScoreSeries(), "terminal_id" : terminal_id});

                // Failure Case
                } else {

                  lisp_output.insert({"message" : "MODEL_FAILURE", "terminal_id" :  terminal_id});

                }
                  // Cleanup TMP File
                  cleanup();
              }));

        }));
      }));
    }));

    return  terminal_id;
  }

  // Meteor Methods
  Meteor.methods({
    'play_game': runModel
  })
