window.racketjs = {
    module: {}
};

window.racketjs.List = function (value, rest) {
    this.rest = rest;
    this.value = value;
};

window.racketjs.Symbol =  function (name) {
    this.name = name;
};

window.racketjs.Values =  function (data) {
    this.data = data;
};

window.racketjs.Char =  function (data) {
    this.data = data;
};

window.racketjs.symbol_QUESTION_ = function (data) {
    return (typeof data) == "object" &&
	data.constructor === window.racketjs.Symbol;
};

window.racketjs.string_QUESTION_ = function (data) {
    return (typeof data) == "string";
};

window.racketjs.boolean_QUESTION_ = function (data) {
    return (typeof data) == "boolean";
};
