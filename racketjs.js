window.racketjs = {
    module: {
	racketjs: {}
    }
};

window.racketjs.List = function (value, rest) {
    this.rest = rest;
    this.value = value;
};

window.racketjs.Pair = function (left, right) {
    this.left = left;
    this.right = right;
};

window.racketjs.Symbol = function (name) {
    this.name = name;
};

window.racketjs.Values = function (data) {
    this.data = data;
};

window.racketjs.Char = function (data) {
    this.data = data;
};

window.racketjs.NULL = function () {};

(function () {
    var exports = window.racketjs.module.racketjs;

    var c = {
	number: function (nb) {
	    if (typeof nb !== 'number') {
		throw new TypeError("number");
	    }
	},

	list: function (elem) {
	    if (!(elem instanceof window.racketjs.List)) {
		throw new TypeError("list");
	    }
	},

	pair: function (elem) {
	    if (!(elem instanceof window.racketjs.Pair)) {
		throw new TypeError("pair");
	    }
	}
    };

    exports.null = new window.racketjs.NULL();

    exports.list = function () {
	var lst = exports.null;

	for (var i = arguments.length - 1; i >= 0; i--) {
	    lst = new window.racketjs.List(arguments[i], lst);
	}

	return lst;
    };

    exports.list_QUESTION_ = function (lst) {
	return lst instanceof window.racketjs.List;
    };

    exports.null_QUESTION_ = function (lst) {
	return exports.null === lst;
    };

    exports.pair_QUESTION_ = function (elem) {
	return elem instanceof window.racketjs.Pair;
    };

    exports.cons = function (left, right) {
	if (right instanceof window.racketjs.List) {
	    return new window.racketjs.List(left, right);
	} else {
	    return new window.racketjs.Pair(left, right);
	}
    };

    exports.car = function (lst) {
	if (lst instanceof window.racketjs.List) {
	    return lst.value;
	} else if (lst instanceof window.racketjs.Pair) {
	    return lst.left;
	}

	throw new TypeError('list|pair');
    };

    exports.cdr = function (lst) {
	if (lst instanceof window.racketjs.List) {
	    return lst.rest;
	} else if (lst instanceof window.racketjs.Pair) {
	    return lst.right;
	}

	throw new TypeError('list|pair');
    };

    exports.symbol_QUESTION_ = function (data) {
	return (typeof data) == "object" &&
	    data.constructor === window.racketjs.module.racketjs.Symbol;
    };

    exports.string_QUESTION_ = function (data) {
	return (typeof data) == "string";
    };

    exports.boolean_QUESTION_ = function (data) {
	return (typeof data) == "boolean";
    };

    exports._PLUS_ = function () {
	var sum = 0;
	for (var i = 0; i < arguments.length; i++) {
	    c.number(arguments[i]);
	    sum += arguments[i];
	}

	return sum;
    };
    
    exports.values = function () {
	if (arguments.length == 1) {
	    return arguments[0];
	} else {
	    return new window.racketjs.Values(arguments);
	}
    };

    exports.not = function (value) {
	return false === value;
    };
})();
