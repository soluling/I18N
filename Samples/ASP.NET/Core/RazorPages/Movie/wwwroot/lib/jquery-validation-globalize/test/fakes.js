window.jQuery = window.$ = {

	// Taken from jQuery
	isNumeric: function( obj ) {
		return !isNaN( parseFloat(obj) ) && isFinite( obj );
	},
	
	validator: {
	
		methods: {
	
			optional: function(element){
				console.log("jQuery.validator.optional called");
				return false; // Nothing is optional in our test scenario
			},

			number: function (value, element) {
				console.log("jQuery.validator.methods.number called");
				throw "the fake of jQuery.validator.methods.number has not been implemented";
			},
			
			date: function (value, element) {
				console.log("jQuery.validator.methods.date called");
				throw "the fake of jQuery.validator.methods.date has not been implemented";
			},
			
			min: function (value, element) {
				console.log("jQuery.validator.methods.min called");
				throw "the fake of jQuery.validator.methods.min has not been implemented";
			},
			
			max: function (value, element) {
				console.log("jQuery.validator.methods.max called");
				throw "the fake of jQuery.validator.methods.max has not been implemented";
			},
			
			range: function (value, element) {
				console.log("jQuery.validator.methods.range called");
				throw "the fake of jQuery.validator.methods.range has not been implemented";
			}
		}
	}
};

window.Globalize = {
	parseFloat: function(value){
		console.log("Globalize.parseFloat called");
		throw "the fake of Globalize.parseFloat has not been implemented";
	},
	
	parseDate: function(value){
		console.log("Globalize.parseDate called");
		throw "the fake of Globalize.parseDate has not been implemented";
	}
};