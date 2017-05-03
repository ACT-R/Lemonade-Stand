
// Meteor Imports
import { Template } from 'meteor/templating';
import { Session } from 'meteor/session';
import { ReactiveVar } from 'meteor/reactive-var';

/**
 ** LAYOUT ELEMENTS
 **/
$(document).ready(function(){

  // Create Tab Structure
  $(".panel-left").tabs();
  $(".panel-left tabs").mouseup(function(){
    window.dispatchEvent(new Event('resize'));
  });

  // Create Draggable Left/Right Elements
  $(".panel-left").resizable({
      autoHide: false,
      handles: 'e',
      resize: function(e, ui)
      {
          var parent = ui.element.parent();
          var remainingSpace = parent.width() - ui.element.outerWidth(),
              divTwo = ui.element.next(),
              divTwoWidth = (remainingSpace - (divTwo.outerWidth() - divTwo.width()))/parent.width()*100+"%";
              divTwo.width(divTwoWidth);
          window.dispatchEvent(new Event('resize'));
      },
      stop: function(e, ui)
      {
          var parent = ui.element.parent();
          ui.element.css(
          {
              width: ui.element.width()/parent.width()*100+"%",
          });
      }
    });

});

/**
 **  BLOCKLY
 **/
workspace = null;
Template.blockly.onRendered(function() {
  var blocklyArea = document.getElementById('blockly_area');
  var blocklyDiv = document.getElementById('blockly_div');

  // Create Workspace
  workspace = Blockly.inject(blocklyDiv,
      {
        media: 'lib/google-blockly/media/',
        toolbox: document.getElementById('toolbox'),
        trashcan: true,
        grid: {
          spacing: 20,
          length: 3,
          colour: '#ccc',
          snap: true
        },
        zoom: {
           controls: true,
           wheel: false,
           startScale: 1.0,
           maxScale: 2,
           minScale: 0.8,
           scaleSpeed: 1.2
        },
      }
  );

  // Listen for Workspace Changes, and Write Code to Codebox
  var onModify = function(event){

    // Regenerate the Code
    var code = "";
    var blocks = workspace.getTopBlocks(true);
    for(var i = 0; i < blocks.length; i++){
      code += Blockly.JavaScript.blockToCode(blocks[i]);
    }

    // Place Inside Tags
    old_code = editor.getValue();
    editor.setValue(old_code.replace(/;;BEGIN-MODEL[\s\S]*?;;END-MODEL/g,
    ";;BEGIN-MODEL\n" + code + "\n  ;;END-MODEL"));


  }
  workspace.addChangeListener(onModify);


  var blockly_resize = function(e) {
    // Compute the absolute coordinates and dimensions of blocklyArea.
    var element = blocklyArea;
    var x = 0;
    var y = 0;
    do {
      x += element.offsetLeft;
      y += element.offsetTop;
      element = element.offsetParent;
    } while (element);
    // Position blocklyDiv over blocklyArea.
    blocklyDiv.style.left = x + 'px';
    blocklyDiv.style.top = y + 'px';
    blocklyDiv.style.width = blocklyArea.offsetWidth + 'px';
    blocklyDiv.style.height = blocklyArea.offsetHeight - 50 + 'px';
  };
  window.addEventListener('resize', blockly_resize, false);
  blockly_resize();
  Blockly.svgResize(workspace);

});

/**
 ** CODE EDITOR
 **/
editor = null;
Template.ace.onRendered(function(){
  editor = ace.edit("ace_editor");
  editor.getSession().setMode("ace/mode/lisp");
  editor.setReadOnly(true);
});

/**
 **  ACT-R COMMAND LINE
 **/

 // Terminal ID
 // Lisp Output Collection
 lisp_output = new Meteor.Collection('lisp_output');
 data_output = new Meteor.Collection('data_output');

 // Output Command Line
 Template.lisp_output.helpers({
   isActive() {
     return Session.get("terminal_id") != null;
   },
   getLines() {
      return lisp_output.find( { terminal_id: Session.get('terminal_id') });
   },
   formatLine(data) {
     if(data != null)
        return new Spacebars.SafeString(data.replace(/(?:\r\n|\r|\n)/g, '<br />'));
     else {
        return new Spacebars.SafeString("");
     }
   }
 });

/**
 **  Load Model Dropdown
 **/
 loadModel = function(url){
   var r = confirm("WARNING!\nThis will discard any work you have in progress.  Do you really wish to load a new model?");
   if(r){
     $.get(url, function(data){
       workspace.clear();
       var xml = Blockly.Xml.textToDom(data);
       Blockly.Xml.domToWorkspace(workspace, xml);
     },"text");
   }
 }
 $(document).ready(function(){
   $(".tabs .load #load_model").change(function(){
     loadModel(this.value);
   })
 })

/**
 **  Save Model Function
 **/
 saveModel = function(){
   var xml = Blockly.Xml.workspaceToDom(workspace);
   var content = Blockly.Xml.domToText(xml)
   uriContent = "data:application/octet-stream," + encodeURIComponent(content);
   newWindow = window.open(uriContent, 'lemonade_model.xml');
 }

/**
**  Run Model Function
**/
runModel = function(iterations = 500){

   Meteor.call('play_game',{model : editor.getValue(), iterations: iterations}, function(err, res){
     if(err || !res){
       console.log(res);
       console.error("Couldn't submit model to server for execution.");
     } else {
       // Wait for Results and Graph

       // Set terminal_id
       Session.set("terminal_id",res);

       // Set Subscriptions
       Tracker.autorun(() => {
         Meteor.subscribe('lisp_output', { terminal_id: Session.get('terminal_id') });
         Meteor.subscribe('data_output', { terminal_id: Session.get('terminal_id') });
      });

      // Observe Changes to Data Output to Create Graph
      data_output.find({ 'terminal_id' : Session.get('terminal_id')}).observe({
        added : function(document){

          // Generate Labels
          var labels = [];
          for(var i = 0; i < document.data.length; i++){
            labels[i] = i;
          }

          // Create Chart
          chart = new Chartist.Line('.ct-chart', {
            labels: labels,
            series: [document.data]
          }, {
            fullWidth: true,
            showPoint: false,
            chartPadding: {
              right: 40
            },
            axisX: {
              labelInterpolationFnc: function(value, index) {
                return index % (Math.ceil(iterations / 10)) === 0 ? value : null;
              }
            }
          });

        }
      });
     }
   });
 }
