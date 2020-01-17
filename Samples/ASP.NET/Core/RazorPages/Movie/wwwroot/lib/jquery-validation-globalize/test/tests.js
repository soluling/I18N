// There will be a better way to test dependencies than creating fakes which throw errors.
// Will investigate possibilities and update test suite when something appropriate found.
// May be worth looking at SinonJS

module("jQuery.validator.methods.number tests");

test( "jQuery.validator.methods.number returns true when a number is successfully parsed", function() {

	Globalize.parseFloat = function(value){
		return 77;
	}

	var result = jQuery.validator.methods.number("a float", "an element");

	strictEqual( result, true );
});

test( "jQuery.validator.methods.number returns false when a number is not successfully parsed", function() {

	Globalize.parseFloat = function(value){
		return NaN;
	}

	var result = jQuery.validator.methods.number("a float", "an element");

	strictEqual( result, false );
});

module("jQuery.validator.methods.date tests");

test( "jQuery.validator.methods.date returns true when a date is successfully parsed", function() {

	Globalize.parseDate = function(value){
		return new Date(2012, 5, 15);
	}

	var result = jQuery.validator.methods.date("a date", "an element");

	strictEqual( result, true );
});

test( "jQuery.validator.methods.date returns false when a date is not successfully parsed", function() {

	Globalize.parseDate = function(value){
		return null;
	}

	var result = jQuery.validator.methods.date("a date", "an element");

	strictEqual( result, false );
});

module("jQuery.validator.methods.min tests");

test( "jQuery.validator.methods.min calls the original min implementation", function() {

	Globalize.parseFloat = function(value){
		return 77;
	}
	
	throws(
    function() {
      jQuery.validator.methods.min("a float", "an element", "param")
    },
    "Our fake jQuery.validator.methods.min has thrown an exception as expected"
  );
});

test( "jQuery.validator.methods.min calls Globalize.parseFloat", function() {

	Globalize.parseFloat = function(value){
		throw "parseFloat has been called";
	}
	
	throws(
    function() {
      jQuery.validator.methods.min("a float", "an element", "param")
    },
    "Our fake Globalize.parseFloat has thrown an exception as expected"
  );
});

module("jQuery.validator.methods.max tests");

test( "jQuery.validator.methods.max calls the original max implementation", function() {

	Globalize.parseFloat = function(value){
		return 77;
	}
	
	throws(
    function() {
      jQuery.validator.methods.max("a float", "an element", "param")
    },
    "Our fake jQuery.validator.methods.max has thrown an exception as expected"
  );
});

test( "jQuery.validator.methods.max calls Globalize.parseFloat", function() {

	Globalize.parseFloat = function(value){
		throw "parseFloat has been called";
	}
	
	throws(
    function() {
      jQuery.validator.methods.max("a float", "an element", "param")
    },
    "Our fake Globalize.parseFloat has thrown an exception as expected"
  );
});

module("jQuery.validator.methods.range tests");

test( "jQuery.validator.methods.range calls the original range implementation", function() {

	Globalize.parseFloat = function(value){
		return 77;
	}
	
	throws(
    function() {
      jQuery.validator.methods.range("a float", "an element", "param")
    },
    "Our fake jQuery.validator.methods.range has thrown an exception as expected"
  );
});

test( "jQuery.validator.methods.range calls Globalize.parseFloat", function() {

	Globalize.parseFloat = function(value){
		throw "parseFloat has been called";
	}
	
	throws(
    function() {
      jQuery.validator.methods.range("a float", "an element", "param")
    },
    "Our fake Globalize.parseFloat has thrown an exception as expected"
  );
});