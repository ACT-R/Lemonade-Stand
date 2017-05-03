
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

Blockly.Blocks['define_chunk_type'] = {
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

    if(Blockly.getMainWorkspace().id === this.workspace.id){
      for(var i = 0; i < 3; i++){
        var isaSymbol = Blockly.getMainWorkspace().newBlock('symbol');
        isaSymbol.setFieldValue("slot_name_"+i,"symbol");
        isaSymbol.initSvg();
        isaSymbol.render();

        this.getInput('ADD'+i).connection.connect(isaSymbol.outputConnection);
      }
    }
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
          .appendField(new Blockly.FieldTextInput("Chunk_Type_Name"), "chunk_type_name");
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

    // Init with ISA Slot
    if(Blockly.getMainWorkspace().id === this.workspace.id){
      var isaName = Blockly.getMainWorkspace().newBlock('symbol');
      isaName.setFieldValue("ISA","symbol");
      isaName.initSvg();
      isaName.render();

      var isaValue = Blockly.getMainWorkspace().newBlock('symbol');
      isaValue.setFieldValue("Chunk_Type","symbol");
      isaValue.initSvg();
      isaValue.render();

      var isaSlot = Blockly.getMainWorkspace().newBlock('slot');
      isaSlot.initSvg();
      isaSlot.render();

      var isaSlotName = isaSlot.getInput('slot_name').connection;
      isaSlotName.connect(isaName.outputConnection);
      var isaSlotValue = isaSlot.getInput('slot_value').connection;
      isaSlotValue.connect(isaValue.outputConnection);

      this.getInput('ADD0').connection.connect(isaSlot.outputConnection);
    }


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

Blockly.Blocks['production_component'] = {
  init: function() {
    var thisBlock = this;

    var image = new Blockly.FieldImage("https://www.gstatic.com/codesite/ph/images/triangle.gif", 15, 15, "*");
    console.log(JSON.stringify(image));

    this.appendDummyInput()
        .appendField(new Blockly.FieldDropdown([["=","="], ["+","+"], ["*","*"], ["@","@"]]), "type")
        .appendField(new Blockly.FieldDropdown([["goal","goal"], ["visual","visual"], ["imaginal","imaginal"], ["motor","motor"], ["retrieval","retrieval"]], function(newType){
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
       case "retrieval":
         this.setColour(160);
         break;
       case "imaginal":
         this.setColour(65);
         break;
       case "motor":
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

    if(Blockly.getMainWorkspace().id === this.workspace.id){
      var slot = Blockly.getMainWorkspace().newBlock('slot');
      slot.initSvg();
      slot.render();

      this.getInput('slot').connection.connect(slot.outputConnection);
    }

  }
};

Blockly.JavaScript['set_chunks'] = function(block) {
  var value_name = Blockly.JavaScript.valueToCode(block, 'NAME', Blockly.JavaScript.ORDER_ATOMIC);
  // TODO: Assemble JavaScript into code variable.
  var code = '...;\n';
  return code;
};

Blockly.JavaScript['production'] = function(block) {
  var text_name = block.getFieldValue('name');
  var text_desc = block.getFieldValue('desc');
  var num_utility = block.getFieldValue('utility');
  var statements_if = Blockly.JavaScript.statementToCode(block, 'IF');
  var statements_then = Blockly.JavaScript.statementToCode(block, 'THEN');

  return "(p "+ text_name + " \"" + text_desc + "\"\n" + statements_if + "==>\n" + statements_then + ")\n" +
         "(spp "+text_name+" :reward "+num_utility+")\n";
};

Blockly.JavaScript['production_component'] = function(block) {
  var dropdown_type = block.getFieldValue('type');
  var dropdown_buffer = block.getFieldValue('buffer');
  var statement_slots = Blockly.JavaScript.statementToCode(block, 'slots');

  return dropdown_type + dropdown_buffer + ">\n" + statement_slots;
};

Blockly.JavaScript['slot_condition'] = function(block) {
  var dropdown_cmp = block.getFieldValue('CMP');
  var slot_code = Blockly.JavaScript.valueToCode(block, 'slot', Blockly.JavaScript.ORDER_ATOMIC);

  if(dropdown_cmp === "="){
    return slot_code;
  } else {
    return dropdown_cmp + " " + slot_code;
  }
};

Blockly.JavaScript['slot'] = function(block){
  var value_slot_name = Blockly.JavaScript.valueToCode(block, 'slot_name', Blockly.JavaScript.ORDER_ATOMIC);
  var value_slot_value = Blockly.JavaScript.valueToCode(block, 'slot_value', Blockly.JavaScript.ORDER_ATOMIC);
  return value_slot_name + " " + value_slot_value + "\n";
}

Blockly.JavaScript['variable'] = function(block) {
  var variable_name = Blockly.JavaScript.variableDB_.getName(block.getFieldValue('variable_name'), Blockly.Variables.NAME_TYPE);
  return ["="+variable_name, Blockly.JavaScript.ORDER_MEMBER];
};

Blockly.JavaScript['symbol'] = function(block) {
  var text_value = block.getFieldValue('symbol');
  return [text_value, Blockly.JavaScript.ORDER_IN];
};
