import { Meteor } from 'meteor/meteor';

 /* Create Databases */
 lisp_output = new Meteor.Collection('lisp_output');
 data_output = new Meteor.Collection('data_output');

/*
 * Cleanup Databases
 */
Meteor.startup(() => {
  // Clear Databases
  lisp_output.remove({});
  data_output.remove({});
});
