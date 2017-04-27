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
    this.setColour(230);
    this.setTooltip('');
    this.setHelpUrl('');
  }
};

Blockly.Blocks['production_component'] = {
  init: function() {
    this.appendDummyInput()
        .appendField(new Blockly.FieldDropdown([["=","="], ["+","+"], ["*","*"], ["@","@"]]), "type")
        .appendField(new Blockly.FieldDropdown([["goal","goal"], ["visual","visual"], ["imaginal","imaginal"], ["motor","motor"], ["retrieval","retrieval"]]), "buffer");
    this.appendStatementInput("slots")
        .setCheck("production_slot");
    this.setPreviousStatement(true, "production_component");
    this.setNextStatement(true, "production_component");
    this.setColour(230);
    this.setTooltip('');
    this.setHelpUrl('');
  }
};

Blockly.Blocks['production_slot'] = {
  init: function() {
    this.appendValueInput("slot_name")
        .setCheck(["symbol", "variable"])
        .appendField(new Blockly.FieldDropdown([["=","="], ["-","-"], [">",">"], [">=",">="], ["<","<"], ["<=","<="]]), "CMP");
    this.appendValueInput("slot_value")
        .setCheck(["symbol", "variable"]);
    this.setInputsInline(true);
    this.setPreviousStatement(true, "production_slot");
    this.setNextStatement(true, "production_slot");
    this.setColour(230);
    this.setTooltip('');
    this.setHelpUrl('');
  }
};
Blockly.Blocks['variable'] = {
  init: function() {
    this.appendDummyInput()
        .appendField("=")
        .appendField(new Blockly.FieldVariable("variable"), "variable");
    this.setOutput(true, null);
    this.setColour(230);
    this.setTooltip('');
    this.setHelpUrl('');
  }
};

Blockly.Blocks['symbol'] = {
  init: function() {
    this.appendDummyInput()
        .appendField(new Blockly.FieldTextInput("symbol"), "symbol");
    this.setOutput(true, null);
    this.setColour(230);
    this.setTooltip('');
    this.setHelpUrl('');
  }
};

Blockly.JavaScript['production'] = function(block) {
  var text_name = block.getFieldValue('name');
  var text_desc = block.getFieldValue('desc');
  var statements_if = Blockly.JavaScript.statementToCode(block, 'IF');
  var statements_then = Blockly.JavaScript.statementToCode(block, 'THEN');

  return "(p "+ text_name + " \"" + text_desc + "\"\n" +
  statements_if +
  "==>\n" +
  statements_then +
  ")\n";
};

Blockly.JavaScript['production_component'] = function(block) {
  var dropdown_type = block.getFieldValue('type');
  var dropdown_buffer = block.getFieldValue('buffer');
  var statement_slots = Blockly.JavaScript.statementToCode(block, 'slots');

  return dropdown_type + dropdown_buffer + ">\n" + statement_slots;
};

Blockly.JavaScript['production_slot'] = function(block) {
  var dropdown_cmp = block.getFieldValue('CMP');
  var value_slot_name = Blockly.JavaScript.valueToCode(block, 'slot_name', Blockly.JavaScript.ORDER_ATOMIC);
  var value_slot_value = Blockly.JavaScript.valueToCode(block, 'slot_value', Blockly.JavaScript.ORDER_ATOMIC);

  if(dropdown_cmp === "="){
    return value_slot_name + " " + value_slot_value + "\n";
  } else {
    return dropdown_cmp + " " + value_slot_name + " " + value_slot_value + "\n";
  }

};

Blockly.JavaScript['variable'] = function(block) {
  var variable_name = Blockly.JavaScript.variableDB_.getName(block.getFieldValue('variable_name'), Blockly.Variables.NAME_TYPE);
  return ["="+variable_name, Blockly.JavaScript.ORDER_MEMBER];
};

Blockly.JavaScript['symbol'] = function(block) {
  var text_value = block.getFieldValue('symbol');
  return [text_value, Blockly.JavaScript.ORDER_IN];
};
