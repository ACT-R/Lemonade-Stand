/**
 **   BLOCK CONFIGURATION
 **/

Blockly.BlockSvg.START_HAT = true;

/**
 **   BLOCK DEFINITIONS
 **/

Blockly.Blocks['variable'] = {
  init: function() {
    this.appendDummyInput()
        .appendField("=")
        .appendField(new Blockly.FieldVariable("variable"), "variable");
    this.setOutput(true, null);
    this.setColour(350);
    this.setTooltip('');
    this.setHelpUrl('');
  }
};

Blockly.Blocks['symbol'] = {
  init: function() {
    this.appendDummyInput()
        .appendField(new Blockly.FieldTextInput("symbol"), "symbol");
    this.setOutput(true, null);
    this.setColour(350);
    this.setTooltip('');
    this.setHelpUrl('');
  }
};

Blockly.Blocks['slot'] = {
  init: function() {
    this.appendValueInput("slot_name")
      .setCheck(["symbol", "variable"]);
    this.appendValueInput("slot_value")
      .setCheck(["symbol", "variable"]);
    this.setOutput(true, "slot");
    this.setInputsInline(true);
    this.setColour(230);
    this.setTooltip('');
    this.setHelpUrl('');
  }
};

Blockly.Blocks['chunk_type'] = {
  /**
   * Block for creating a list with any number of elements of any type.
   * @this Blockly.Block
   */
  init: function() {
    this.itemCount_ = 3;
    this.updateShape_();
    this.setMutator(new Blockly.Mutator(['lists_create_with_item']));
    this.setColour(160);
    this.setHelpUrl("");
    this.setTooltip("");
  },
  /**
   * Create XML to represent list inputs.
   * @return {!Element} XML storage element.
   * @this Blockly.Block
   */
  mutationToDom: function() {
    var container = document.createElement('mutation');
    container.setAttribute('items', this.itemCount_);
    return container;
  },
  /**
   * Parse XML to restore the list inputs.
   * @param {!Element} xmlElement XML storage element.
   * @this Blockly.Block
   */
  domToMutation: function(xmlElement) {
    this.itemCount_ = parseInt(xmlElement.getAttribute('items'), 10);
    this.updateShape_();
  },
  /**
   * Populate the mutator's dialog with this block's components.
   * @param {!Blockly.Workspace} workspace Mutator's workspace.
   * @return {!Blockly.Block} Root block in mutator.
   * @this Blockly.Block
   */
  decompose: function(workspace) {
    var containerBlock = workspace.newBlock('lists_create_with_container');
    containerBlock.initSvg();
    var connection = containerBlock.getInput('STACK').connection;
    for (var i = 0; i < this.itemCount_; i++) {
      var itemBlock = workspace.newBlock('lists_create_with_item');
      itemBlock.initSvg();
      connection.connect(itemBlock.previousConnection);
      connection = itemBlock.nextConnection;
    }
    return containerBlock;
  },
  /**
   * Reconfigure this block based on the mutator dialog's components.
   * @param {!Blockly.Block} containerBlock Root block in mutator.
   * @this Blockly.Block
   */
  compose: function(containerBlock) {
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    // Count number of inputs.
    var connections = [];
    while (itemBlock) {
      connections.push(itemBlock.valueConnection_);
      itemBlock = itemBlock.nextConnection &&
          itemBlock.nextConnection.targetBlock();
    }
    // Disconnect any children that don't belong.
    for (var i = 0; i < this.itemCount_; i++) {
      var connection = this.getInput('ADD' + i).connection.targetConnection;
      if (connection && connections.indexOf(connection) == -1) {
        connection.disconnect();
      }
    }
    this.itemCount_ = connections.length;
    this.updateShape_();
    // Reconnect any child blocks.
    for (var i = 0; i < this.itemCount_; i++) {
      Blockly.Mutator.reconnect(connections[i], this, 'ADD' + i);
    }
  },
  /**
   * Store pointers to any connected child blocks.
   * @param {!Blockly.Block} containerBlock Root block in mutator.
   * @this Blockly.Block
   */
  saveConnections: function(containerBlock) {
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    var i = 0;
    while (itemBlock) {
      var input = this.getInput('ADD' + i);
      itemBlock.valueConnection_ = input && input.connection.targetConnection;
      i++;
      itemBlock = itemBlock.nextConnection &&
          itemBlock.nextConnection.targetBlock();
    }
  },
  /**
   * Modify this block to have the correct number of inputs.
   * @private
   * @this Blockly.Block
   */
  updateShape_: function() {
    if (this.itemCount_ && this.getInput('EMPTY')) {
      this.removeInput('EMPTY');
    } else if (!this.itemCount_ && !this.getInput('EMPTY')) {
      this.appendDummyInput('EMPTY')
          .appendField(new Blockly.FieldTextInput("Chunk_Type_Name"), "chunk_type_name")

    }
    // Add new inputs.
    for (var i = 0; i < this.itemCount_; i++) {
      if (!this.getInput('ADD' + i)) {
        var input = this.appendValueInput('ADD' + i)
                        .setCheck(["symbol"]);
        if (i == 0) {
          input.appendField(new Blockly.FieldTextInput("Chunk_Type_Name"), "chunk_type_name");
        }
      }
    }
    // Remove deleted inputs.
    while (this.getInput('ADD' + i)) {
      this.removeInput('ADD' + i);
      i++;
    }
  }
};

Blockly.Blocks['chunk'] = {
  /**
   * Block for creating a list with any number of elements of any type.
   * @this Blockly.Block
   */
  init: function() {
    this.itemCount_ = 3;
    this.updateShape_();
    this.setMutator(new Blockly.Mutator(['lists_create_with_item']));
    this.setColour(20);
    this.setHelpUrl("");
    this.setTooltip("");

  },
  /**
   * Create XML to represent list inputs.
   * @return {!Element} XML storage element.
   * @this Blockly.Block
   */
  mutationToDom: function() {
    var container = document.createElement('mutation');
    container.setAttribute('items', this.itemCount_);
    return container;
  },
  /**
   * Parse XML to restore the list inputs.
   * @param {!Element} xmlElement XML storage element.
   * @this Blockly.Block
   */
  domToMutation: function(xmlElement) {
    this.itemCount_ = parseInt(xmlElement.getAttribute('items'), 10);
    this.updateShape_();
  },
  /**
   * Populate the mutator's dialog with this block's components.
   * @param {!Blockly.Workspace} workspace Mutator's workspace.
   * @return {!Blockly.Block} Root block in mutator.
   * @this Blockly.Block
   */
  decompose: function(workspace) {
    var containerBlock = workspace.newBlock('lists_create_with_container');
    containerBlock.initSvg();
    var connection = containerBlock.getInput('STACK').connection;
    for (var i = 0; i < this.itemCount_; i++) {
      var itemBlock = workspace.newBlock('lists_create_with_item');
      itemBlock.initSvg();
      connection.connect(itemBlock.previousConnection);
      connection = itemBlock.nextConnection;
    }
    return containerBlock;
  },
  /**
   * Reconfigure this block based on the mutator dialog's components.
   * @param {!Blockly.Block} containerBlock Root block in mutator.
   * @this Blockly.Block
   */
  compose: function(containerBlock) {
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    // Count number of inputs.
    var connections = [];
    while (itemBlock) {
      connections.push(itemBlock.valueConnection_);
      itemBlock = itemBlock.nextConnection &&
          itemBlock.nextConnection.targetBlock();
    }
    // Disconnect any children that don't belong.
    for (var i = 0; i < this.itemCount_; i++) {
      var connection = this.getInput('ADD' + i).connection.targetConnection;
      if (connection && connections.indexOf(connection) == -1) {
        connection.disconnect();
      }
    }
    this.itemCount_ = connections.length;
    this.updateShape_();
    // Reconnect any child blocks.
    for (var i = 0; i < this.itemCount_; i++) {
      Blockly.Mutator.reconnect(connections[i], this, 'ADD' + i);
    }
  },
  /**
   * Store pointers to any connected child blocks.
   * @param {!Blockly.Block} containerBlock Root block in mutator.
   * @this Blockly.Block
   */
  saveConnections: function(containerBlock) {
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    var i = 0;
    while (itemBlock) {
      var input = this.getInput('ADD' + i);
      itemBlock.valueConnection_ = input && input.connection.targetConnection;
      i++;
      itemBlock = itemBlock.nextConnection &&
          itemBlock.nextConnection.targetBlock();
    }
  },
  /**
   * Modify this block to have the correct number of inputs.
   * @private
   * @this Blockly.Block
   */
  updateShape_: function() {
    if (this.itemCount_ && this.getInput('EMPTY')) {
      this.removeInput('EMPTY');
    } else if (!this.itemCount_ && !this.getInput('EMPTY')) {
      this.appendDummyInput('EMPTY')
           .appendField(new Blockly.FieldTextInput("Chunk_Name"), "chunk_name");
    }
    // Add new inputs.
    for (var i = 0; i < this.itemCount_; i++) {
      if (!this.getInput('ADD' + i)) {
        var input = this.appendValueInput('ADD' + i)
                        .setCheck(["slot"]);
        if (i == 0) {
          input.appendField(new Blockly.FieldTextInput("Chunk_Name"), "chunk_name");
        }
      }
    }
    // Remove deleted inputs.
    while (this.getInput('ADD' + i)) {
      this.removeInput('ADD' + i);
      i++;
    }
  }
};

Blockly.Blocks['lists_create_with_container'] = {
  /**
   * Mutator block for list container.
   * @this Blockly.Block
   */
  init: function() {
    this.appendDummyInput()
        .appendField("Chunk");
    this.appendStatementInput('STACK');
    this.setColour(260);
    this.setTooltip("");
    this.contextMenu = false;
  }
};

Blockly.Blocks['lists_create_with_item'] = {
  /**
   * Mutator block for adding items.
   * @this Blockly.Block
   */
  init: function() {
    this.appendDummyInput()
        .appendField("Slot");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(230);
    this.setTooltip("");
    this.contextMenu = false;
  }
};

Blockly.Blocks['production'] = {
  init: function() {
    this.appendDummyInput()
        .appendField(new Blockly.FieldTextInput("Production Name"), "name");
    this.appendDummyInput()
        .appendField(new Blockly.FieldTextInput("Production Rule Description"), "desc");
    this.appendStatementInput("IF")
        .setCheck("production_component");
    this.appendStatementInput("THEN")
        .setCheck("production_component");
    this.appendDummyInput()
        .appendField("Utility")
        .appendField(new Blockly.FieldTextInput("nil"), "utility");
    this.setColour(230);
    this.setTooltip('');
    this.setHelpUrl('');
  }
};

// Helper Function for Creating Slot Conditions
var create_slot_condition = function(comparison, key, value, connection){
  var key_block = Blockly.getMainWorkspace().newBlock('symbol');
  key_block.setFieldValue(key,"symbol");
  key_block.initSvg();
  key_block.render();

  var value_block = Blockly.getMainWorkspace().newBlock('symbol');
  value_block.setFieldValue(value,"symbol");
  value_block.initSvg();
  value_block.render();

  var slot = Blockly.getMainWorkspace().newBlock('slot');
  slot.initSvg();
  slot.render();

  var cond = Blockly.getMainWorkspace().newBlock('slot_condition');
  cond.setFieldValue(comparison,"CMP");
  cond.initSvg();
  cond.render();

  slot.getInput('slot_name').connection.connect(key_block.outputConnection);
  slot.getInput('slot_value').connection.connect(value_block.outputConnection);
  cond.getInput('slot').connection.connect(slot.outputConnection);

  connection.connect(cond.previousConnection);
}

Blockly.Blocks['production_component'] = {
  init: function() {
    var thisBlock = this;

    /* "Magic" Generation Function */
    var image = new Blockly.FieldImage("lib/modernuiicons/WindowsPhone/dark/appbar.camera.flash.selected.png", 17, 17, "*", function(){

      // Verify not in Toolbox
      if (Blockly.getMainWorkspace().id === thisBlock.workspace.id){

        // Verify if existing slots should be deleted.
        if(!"targetConnection" in thisBlock.getInput('slots').connection ||
           thisBlock.getInput('slots').connection.targetConnection == null ||
           confirm("WARNING!\nThis will delete all slots in this block.  Do you wish to do this?")
          ){

            // Delete Existing
            if(thisBlock.getInput("slots").connection.targetConnection != null){
              thisBlock.getInput("slots").connection.targetConnection.sourceBlock_.dispose();
            }

            switch(thisBlock.getFieldValue('buffer')){

              case "goal":
                switch (thisBlock.getFieldValue('type')){
                   case "=":
                     create_slot_condition("=","state","purchase",thisBlock.getInput('slots').connection);
                     create_slot_condition("=","isa","game-state",thisBlock.getInput('slots').connection);
                     break;
                   default:
                     Alert("This is an atypical buffer request.");
                     break;
                }
                break;

              case "visual":
                switch (thisBlock.getFieldValue('type')){
                   case "?":
                     create_slot_condition("=","state","free",thisBlock.getInput('slots').connection);
                     break;
                }
                break;

              case "imaginal":
                switch (thisBlock.getFieldValue('type')){
                   case "?":
                     create_slot_condition("=","state","free",thisBlock.getInput('slots').connection);
                     break;
                   case "+":
                     break;
                   case "@":
                     break;
                   default:
                     Alert("This is an atypical buffer request.");
                     break;
                }
                break;

              case "manual":
                switch (thisBlock.getFieldValue('type')){
                   case "?":
                     create_slot_condition("=","state","free",thisBlock.getInput('slots').connection);
                     break;

                   case "+":
                     create_slot_condition("=","key","l",thisBlock.getInput('slots').connection);
                     create_slot_condition("=","cmd","press-key",thisBlock.getInput('slots').connection);
                     break;

                   default:
                     Alert("This is an atypical buffer request.");
                     break;
                }
                break;

              case "retrieval":

            }

        }
      }
    });

    this.appendDummyInput()
        .appendField(new Blockly.FieldDropdown([["=","="], ["?","?"], ["+","+"], ["*","*"], ["@","@"]]), "type")
        .appendField(new Blockly.FieldDropdown([["goal","goal"], ["visual","visual"], ["imaginal","imaginal"], ["manual","manual"], ["retrieval","retrieval"]], function(newType){
            thisBlock._updateBuffer(newType);
        }), "buffer")
        .appendField(image);

    this.appendStatementInput("slots")
        .setCheck("slot_condition");
    this.setPreviousStatement(true, "production_component");
    this.setNextStatement(true, "production_component");
    this.setColour(230);
    this.setTooltip('');
    this.setHelpUrl('');

    this._updateBuffer("goal");

  },

  /**
   ** Update based on Buffer Change
   **/
  _updateBuffer: function(newType){
     switch(newType){
       case "goal":
         this.setColour(290);
         break;
       case "visual":
         this.setColour(120);
         break;
       case "visual-location":
         this.setColour(120);
         break;
       case "retrieval":
         this.setColour(160);
         break;
       case "imaginal":
         this.setColour(65);
         break;
       case "manual":
         this.setColour(330);
         break;
     }
  }
};

Blockly.Blocks['slot_condition'] = {
  init: function() {
    this.appendValueInput("slot")
        .setCheck(["slot"])
        .appendField(new Blockly.FieldDropdown([["=","="], ["-","-"], [">",">"], [">=",">="], ["<","<"], ["<=","<="]]), "CMP");
    this.setInputsInline(true);
    this.setPreviousStatement(true, "slot_condition");
    this.setNextStatement(true, "slot_condition");
    this.setColour(120);
    this.setTooltip('');
    this.setHelpUrl('');

  }
};

/**
 **   CODE GENERATION
 **/

 Blockly.JavaScript['chunk'] = function(block) {
   var chunk_name = block.getFieldValue('chunk_name').toString().replace(/ /g,"_");

   var code = "\t\t(" + chunk_name +" ";
   for(var i = 0; i < block.itemCount_; i++){
     if(block.getInput('ADD'+i).connection.targetConnection != null)
      code += Blockly.JavaScript.valueToCode(block, 'ADD'+i, Blockly.JavaScript.ORDER_NONE);
   }

   return code + ")";
 };

Blockly.JavaScript['chunk_type'] = function(block) {
  var chunk_type_name = block.getFieldValue('chunk_type_name').toString().replace(/ /g,"_");

  var code = "(chunk-type "+chunk_type_name+" ";
  for(var i = 0; i < block.itemCount_; i++){
    code += Blockly.JavaScript.valueToCode(block, 'ADD'+i, Blockly.JavaScript.ORDER_NONE)+" ";
  }

  return code + ") \n";
};

Blockly.JavaScript['production'] = function(block) {
  var text_name = block.getFieldValue('name').toString().replace(/ /g,"_");
  var text_desc = block.getFieldValue('desc');
  var num_utility = block.getFieldValue('utility');
  var statements_if = Blockly.JavaScript.statementToCode(block, 'IF');
  var statements_then = Blockly.JavaScript.statementToCode(block, 'THEN');

  return "(p "+ text_name + " \"" + text_desc + "\"\n" + statements_if + "\n==>\n" + statements_then + ")\n" +
         "(spp "+text_name+" :reward "+num_utility+")\n";
};

Blockly.JavaScript['production_component'] = function(block) {
  var dropdown_type = block.getFieldValue('type');
  var dropdown_buffer = block.getFieldValue('buffer');
  var statement_slots = Blockly.JavaScript.statementToCode(block, 'slots');

  return dropdown_type + dropdown_buffer + ">\n" + statement_slots+"\n";
};

Blockly.JavaScript['slot_condition'] = function(block) {
  var dropdown_cmp = block.getFieldValue('CMP');
  var slot_code = Blockly.JavaScript.valueToCode(block, 'slot', Blockly.JavaScript.ORDER_NONE);

  if(dropdown_cmp === "="){
    return slot_code+"\n";
  } else {
    return dropdown_cmp + " " + slot_code+"\n";
  }
};

Blockly.JavaScript['slot'] = function(block){
  var value_slot_name = Blockly.JavaScript.valueToCode(block, 'slot_name', Blockly.JavaScript.ORDER_NONE);
  var value_slot_value = Blockly.JavaScript.valueToCode(block, 'slot_value', Blockly.JavaScript.ORDER_NONE);
  return [value_slot_name + " " + value_slot_value, Blockly.JavaScript.ORDER_NONE];
}

Blockly.JavaScript['variable'] = function(block) {
  var variable_name = Blockly.JavaScript.variableDB_.getName(block.getFieldValue('variable_name'), Blockly.Variables.NAME_TYPE);
  return ["="+variable_name, Blockly.JavaScript.ORDER_NONE];
};

Blockly.JavaScript['symbol'] = function(block) {
  var text_value = block.getFieldValue('symbol');
  return [text_value, Blockly.JavaScript.ORDER_NONE];
};
