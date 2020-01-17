

	var arrayFilter = function( array, callback ) {
		var i, length,
			newArray = [];
		if ( array.filter ) {
			return array.filter( callback );
		}
		for ( i = 0, length = array.length; i < length; i++ ) {
			if (callback( array[ i ], i, array )) {
				newArray.push(array[ i ]);
			}
		}
		return newArray;
	};

