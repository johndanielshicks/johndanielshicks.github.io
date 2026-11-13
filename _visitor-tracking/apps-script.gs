/**
 * Google Apps Script that receives visits from the website and appends them to a Sheet.
 * Paste this into Extensions > Apps Script of your "Website Visitors" Google Sheet.
 * See README.md in this folder for setup steps.
 */

var SHEET_NAME = 'Visits';
var HEADERS = ['Timestamp', 'IP', 'City', 'Region', 'Country', 'ISP / Org',
               'Page', 'Referrer', 'User agent', 'Screen', 'Language', 'Time zone'];

function doPost(e) {
  var d = {};
  try { d = JSON.parse(e.postData.contents); } catch (err) { return out_('bad request'); }

  var sheet = getSheet_();
  sheet.appendRow([
    new Date(),
    clean_(d.ip), clean_(d.city), clean_(d.region), clean_(d.country), clean_(d.org),
    clean_(d.page), clean_(d.referrer), clean_(d.userAgent),
    clean_(d.screen), clean_(d.language), clean_(d.timezone)
  ]);
  return out_('ok');
}

// Lets you open the /exec URL in a browser to confirm the deployment works.
function doGet() {
  return out_('Visitor logger is running.');
}

function getSheet_() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = ss.getSheetByName(SHEET_NAME) || ss.insertSheet(SHEET_NAME);
  if (sheet.getLastRow() === 0) {
    sheet.appendRow(HEADERS);
    sheet.setFrozenRows(1);
  }
  return sheet;
}

// The endpoint is public, so cap lengths and neutralise spreadsheet formulas.
function clean_(v) {
  var s = String(v == null ? '' : v).slice(0, 500);
  return /^[=+\-@]/.test(s) ? "'" + s : s;
}

function out_(text) {
  return ContentService.createTextOutput(text).setMimeType(ContentService.MimeType.TEXT);
}
