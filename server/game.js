/**
 **  Lemonade Stand Game
 **  @author Derek Brown
 **
 **/

/* WEATHER REPORTS */
WeatherCondition = {
   SUNNY : 1,
   CLOUDY : 2,
   RAINY : 3,
   SNOWY : 4
};

class WeatherReport{
  constructor(temp = 85, cond = WeatherCondition.SUNNY){
    this.temp = temp;
    this.cond = cond;
  }

  getTemp(){
    return this.temp.toString();
  }

  getCond(){
    switch(this.cond){
      case WeatherCondition.SUNNY:
         return "Sunny";
      case WeatherCondition.CLOUDY:
         return "Cloudy";
      case WeatherCondition.RAINY:
         return "Rainy"
      case WeatherCondition.SNOWY:
         return "Snowy";
    }
  }

  toString(){
    return this.getTemp() + " " + this.getCond();
  }

  /**
   ** getNextDay
   ** create the next day's weather report through a simple
   ** markov model.
   **/
  getNextDay(){
     var shift = Math.random();
     var next_cond = this.cond;
     var next_temp;

     // Shift Condition
     if(shift < 1/3 && this.cond != WeatherCondition.SUNNY)
        next_cond = this.cond - 1;
     else if(shift > 1/3 && this.cond != WeatherCondition.SNOWY)
        next_cond = this.cond + 1;

     // Create new temperature
     switch(next_cond){
       case WeatherCondition.SUNNY:
          next_temp = 70 + (Math.random() * 32);
          break;
       case WeatherCondition.CLOUDY:
          next_temp = 60 + (Math.random() * 25);
          break;
       case WeatherCondition.RAINY:
          next_temp = 40 + (Math.random() * 30);
          break;
       case WeatherCondition.SNOWY:
          next_temp = 20 + (Math.random() * 20);
          break;
     }

     // Round to Temperature
     return new WeatherReport(Math.round(next_temp), next_cond);
  }

  /**
   ** getSalesPotential
   ** return an integer representing the sales potential,
   ** the number of possible people that could be sold.
   **/
  getSalesPotential(){
     switch(this.cond){
       case WeatherCondition.SUNNY:
          return this.temp * 1.2;
       case WeatherCondition.CLOUDY:
          return this.temp;
       case WeatherCondition.RAINY:
          return this.temp * 0.6;
       case WeatherCondition.SNOWY:
          return this.temp * 0.2;
     }
  }
}

/* INVENTORY */
recipe_ratio = [0.4, 0.2, 3.0, 1.0];
purchase_quantity = [100, 50, 1000, 200];
purchase_cost = [20, 20, 10, 30];
game_events = {
  ICE_MELT : 1,
  LEMON_SPOIL : 2,
  SUGAR_SPOIL : 3
};

class Inventory{

    constructor(lemons = 0, sugar = 0, ice = 0, cups = 0, events = []){
       this.lemons = lemons;
       this.sugar = sugar;
       this.ice = ice;
       this.cups = cups;
       this.events = events;
    }

    toString(){
      return this.getEventsString() +
             "Inventory: \n" +
             "L: "+this.lemons+
            " S: "+this.sugar+
            " I: "+this.ice+
            " C: "+this.cups;
    }


    getSalesMade(sales_potential){

      var limit_ingredient = [this.lemons / recipe_ratio[0], this.sugar / recipe_ratio[1], this.ice / recipe_ratio[2], this.cups / recipe_ratio[3]];
      var recipe_potential = Math.floor(Math.min.apply(Math.max(), limit_ingredient));

      return Math.floor(Math.min(sales_potential, recipe_potential));
    }

    getNextDay(sales_made){
      // Subtract Made Cups
        var new_lemons = this.lemons - (sales_made * recipe_ratio[0]);
        var new_sugar = this.sugar  - (sales_made * recipe_ratio[1]);
        var new_cups = this.cups - (sales_made * recipe_ratio[3]);

      // Compute Spoils
        var new_events = [];
        // Lemons Spoil

        if(Math.random() > 2/3){
          new_lemons = this.lemons * 2/3;
          new_events.push(game_events.LEMONS_SPOIL);
        }

        // Sugar Spoils
        if(Math.random() > 6/8){
          new_sugar = this.sugar * 1/2;
          new_events.push(game_events.SUGAR_SPOIL);
        }

        // Create new Inventory
        // Ice Melts, Cups aren't Damaged
        new_events.push(game_events.ICE_MELT);
        return new Inventory(Math.floor(new_lemons), Math.floor(new_sugar), 0, Math.floor(new_cups), new_events);

    }

    getEventsString(){
       var res = "Events:\n";
       for(var i = 0; i < this.events.length; i++){
         switch(this.events[i]){
           case game_events.ICE_MELT:
              res += "Ice melted! \n";
              break;
           case game_events.LEMON_SPOIL:
              res += "Lemons spoiled! \n";
              break;
           case game_events.SUGAR_SPOIL:
              res += "Sugar spoiled! \n";
              break;
         }
       }

       if(this.events.length != 0){
          res = res + "\n";
       } else {
          res = res + "None.\n\n";
       }

       return res;
    }
}

/* GAME STATE */
class GameState{

  constructor(){
    this.day = 1;
    this.score = [0];
    this.cups_sold = [0];
    this.events = [[]];
    this.weather = [new WeatherReport()];
    this.inventory = [new Inventory()];
  }

  toString(){
    return "\n\n===  DAY "+this.day+" ===" + "\n" +
           "Cups Sold: \n"+this.getCupsSold() +"\n\n"+
           this.getInventory().toString() + "\n\n"+
           "Weather: \n" + this.getWeather().toString() + "\n\n"+
           "Score: \n" + "$" + this.getScore().toString() + "\n";
  }

  getDay(){
    return this.inventory.length;
  }

  getInventory(){
    return this.inventory[this.inventory.length - 1];
  }

  getWeather(){
    return this.weather[this.weather.length - 1];
  }

  getScore(){
    return this.score[this.score.length - 1];
  }

  getScoreSeries(){
    return this.score;
  }

  getCupsSold(){
    return this.cups_sold[this.cups_sold.length - 1];
  }

  addInventory(inventory){
    this.inventory.push(inventory);
  }

  addWeather(weather){
    this.weather.push(weather);
  }

  addScore(score){
    this.score.push(score);
  }

  addCupsSold(cups_sold){
    this.cups_sold.push(cups_sold);
  }

  nextDay(moves){

    // Check Moves Length
    if(moves.length != 4){
      throw "Moves must be of length 4";
    }

    // Update Weather Forecast
    var new_weather = this.getWeather().getNextDay();

    // Construct New Inventory
    var new_inventory = this.getInventory();
    var cost = 0;

    if(moves[0]){
      new_inventory.lemons += purchase_quantity[0];
      cost -= purchase_cost[0];
    }

    if(moves[1]){
      new_inventory.sugar += purchase_quantity[1];
      cost -= purchase_cost[1];
    }

    if(moves[2]){
      new_inventory.ice += purchase_quantity[2];
      cost -= purchase_cost[2];
    }

    if(moves[3]){
      new_inventory.cups += purchase_quantity[3];
      cost -= purchase_cost[3];
    }

    // Sell Some Lemonade
    var cups_sold = new_inventory.getSalesMade(this.getWeather().getSalesPotential());

    // Save Weather, Inventory, Score
    this.addWeather(new_weather);
    this.addInventory(new_inventory.getNextDay(cups_sold));
    this.addScore(this.getScore() + cups_sold + cost);
    this.addCupsSold(cups_sold);
    this.day += 1;
  }
}

playInteractive = function(){

  // Load Dependencies
  console.log("Welcome to Lemonade Stand!");
  var inquirer = require('inquirer');

  // Create GameState
  game = new GameState();

  // Game Loop
  var game_loop = function(){

    // Print Game State
    console.log(game.toString()+"\n");

    // Prompt for Buying
    inquirer.prompt([
      {
        type: 'checkbox',
        name: 'purchases',
        message: 'What would you like to purchase?',
        choices: [
          {
            name: purchase_quantity[0] + ' Lemons' + " - $"+purchase_cost[0],
            key: 'l',
            value: 'l',
          },
          {
            name: purchase_quantity[1] + ' Sugar Cubes' + " - $"+purchase_cost[1],
            key: 's',
            value: 's',
          },
          {
            name: purchase_quantity[2] + ' Ice Cubes' + " - $"+purchase_cost[2],
            key: 'i',
            value: 'i',
          },
          {
            name: purchase_quantity[3] + ' Cups' + " - $"+purchase_cost[3],
            key: 'c',
            value: 'c',
          }
        ]

      }
    ]).then(function (answers) {

      // Convert to Play String
      var p = answers.purchases;
      var play = [
        p.includes('l'),
        p.includes('s'),
        p.includes('i'),
        p.includes('c')
      ];

      // Go to Next Day
      game.nextDay(play);
      game_loop();

    });

  }
  game_loop();

  return;
}

module.exports = {
  play : playInteractive,
  game: GameState
}
