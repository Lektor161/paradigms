"use strict";

const varNameToIndex = {
    'x': 0,
    'y': 1,
    'z': 2
};

const Const = function (value) {
    this._value = value;
};
Const.prototype.evaluate = function () { return this._value; };
Const.prototype.toString = function () { return this._value.toString(); };
Const.prototype.diff = () => ZERO;
Const.prototype.prefix = function () { return this._value.toString(); };
Const.prototype.postfix = function () { return this._value.toString(); };

const ZERO = new Const(0);
const ONE = new Const(1);
const E = new Const(Math.E);

const Variable = function (name) {
    this._name = name;
    this._id = varNameToIndex[name];
};
Variable.prototype.evaluate = function (...args) { return args[this._id]; };
Variable.prototype.toString = function () { return this._name; };
Variable.prototype.diff = function (diffName) { return diffName === this._name ? ONE : ZERO; };
Variable.prototype.prefix = function () { return this._name; };
Variable.prototype.postfix = function () { return this._name; };

function Operation(...opers) {
    this._opers = opers;
};
Operation.prototype.evaluate = function (...args) {
    return this._calcFunction(...this._opers.map(oper => oper.evaluate(...args)));
};
Operation.prototype.toString = function () {
    return this._opers.join(' ') + ' ' + this._name;
};
Operation.prototype.diff = function (diffName) {
    return this._diffFunction(diffName, ...this._opers);
};
Operation.prototype.getArity = function () { return this._calcFunction.length; };
Operation.prototype.prefix = function () {
    return '(' + this._name + ' ' + this._opers.map(oper => oper.prefix()).join(' ') + ')';
}
Operation.prototype.postfix = function () {
    return '(' + this._opers.map(oper => oper.postfix()).join(' ') + ' ' + this._name + ')';
}

const makeOperation = function(name, calcFunction, diffFunction) {
    let Constructor = function (...opers) {
        Operation.apply(this, opers);
    };
    Constructor.prototype = Object.create(Operation.prototype);
    Constructor.prototype.constructor = Operation;
    Constructor.prototype._name = name;
    Constructor.prototype._calcFunction = calcFunction;
    Constructor.prototype._diffFunction = diffFunction;
    return Constructor;
};

const Add = makeOperation('+', (a, b) => a + b, (name, a, b) => new Add(a.diff(name), b.diff(name)));
const Subtract = makeOperation('-', (a, b) => a - b, (name, a, b) => new Subtract(a.diff(name), b.diff(name)));
const Multiply = makeOperation('*', (a, b) => a * b,
    (name, a, b) => new Add(new Multiply(a, b.diff(name)), new Multiply(a.diff(name), b))
);
const Divide = makeOperation('/', (a, b) => a / b,
    (name, a, b) => new Divide(
        new Subtract(new Multiply(a.diff(name), b), new Multiply(a, b.diff(name))),
        new Multiply(b, b)
    )
);
const Negate = makeOperation('negate', a => -a, (name, a) => new Negate(a.diff(name)));
const Power = makeOperation(
    'pow',
    (a, b) => Math.pow(a, b),
    function (name, a, b) {
        return new Multiply(
            new Add(
                new Multiply(b.diff(name), new Log(E, a)),
                new Divide(new Multiply(a.diff(name), b), a)
            ),
            this
        )
    }
);
const Log = makeOperation(
    'log',
    (a, b) => Math.log(Math.abs(b)) / Math.log(Math.abs(a)),
    (name, a, b) => {
        let LnA = new Log(E, a);
        let LnB = new Log(E, b);
        return new Divide(
            new Subtract(
                new Divide(new Multiply(b.diff(name), LnA), b),
                new Divide(new Multiply(a.diff(name), LnB), a)
            ),
            new Multiply(LnA, LnA)
        )
    }
);

const sumExp = (...args) => args.reduce((res, cur) => res + Math.exp(cur), 0);
const Sumexp = makeOperation(
    'sumexp', sumExp,
    (name, ...args) => args.reduce((res, cur) => new Add(res, new Multiply(cur.diff(name), new Sumexp(cur))), ZERO)
);

const Softmax = makeOperation(
    'softmax',
    (...args) => Math.exp(args[0]) / sumExp(...args),
    (diffName, ...args) => {
        return (new Divide(new Sumexp(args[0]), new Sumexp(...args))).diff(diffName);
    }
);

const OPERATIONS = {
    '+': Add,
    '-': Subtract,
    '*': Multiply,
    '/': Divide,
    'negate': Negate,
    'pow': Power,
    'log': Log,
    'sumexp': Sumexp,
    'softmax': Softmax
};


const parse = (str) => {
    let stack = [];
    let tokens = str.split(' ').filter(word => word.length > 0);
    for (let token of tokens) {
        if (token in OPERATIONS) {
            let operation = OPERATIONS[token];
            let arr = stack.splice(-operation.prototype.getArity());
            stack.push(new operation(...arr));
        } else if (token in varNameToIndex) {
            stack.push(new Variable(token));
        } else {
            stack.push(new Const(parseInt(token)));
        }
    }
    return stack.pop();
};

function ParseError(message) {
    this.message = message;
}
ParseError.prototype = Object.create(Error.prototype);
ParseError.prototype.constructor = ParseError;
ParseError.prototype.name = "ParseError";

const isDigit = c => '0' <= c && c <= '9';
const isNumber = str => {
    if (isDigit(str[0]) || str[0] === '-' && isDigit(str[1])) {
        for (let i = 1; i < str.length; i++) {
            if (!isDigit(str[i])) {
                return false;
            }
        }
        return true;
    }
    return false;
};

function Parser(expression, parseBracket) {
    this.parseBracket = parseBracket;
    this.expression = expression;
    this.curPos = 0;
    this.curToken = '';
};
Parser.prototype.skipWhiteSpace = function() {
    while (this.curPos < this.expression.length && this.expression[this.curPos] === ' ') {
        this.curPos++;
    }
};

Parser.prototype.getTokenPos = function() {
    return this.curPos - this.curToken.length + 1;
}

Parser.prototype.nextToken = function () {
    this.skipWhiteSpace();
    let res;
    if (this.expression[this.curPos] === '(' || this.expression[this.curPos] === ')') {
        res = this.expression[this.curPos];
        this.curPos++;
    } else {
        let start = this.curPos;
        while (this.curPos < this.expression.length && this.expression[this.curPos] !== ' ' &&
        this.expression[this.curPos] !== '(' && this.expression[this.curPos] !== ')') {
            this.curPos++;
        }
        res = this.expression.slice(start, this.curPos);
    }
    this.curToken = res;
};

Parser.prototype.parseOperationArgs = function() {
    let stack = [];
    while (this.curPos < this.expression.length && this.curToken !== ')' && !(this.curToken in OPERATIONS)) {
        stack.push(this.parse());
        this.nextToken();
    }
    return stack;
};

Parser.prototype.getOperation = function() {
    if (this.curToken in OPERATIONS) {
        let operation = OPERATIONS[this.curToken];
        this.nextToken();
        return operation;
    } else {
        throw new ParseError('expected operation at pos: ' + this.getTokenPos() + ', but found \'' + this.curToken + '\'');
    }
};

Parser.prototype.checkAndOfBracket = function(operation, stack) {
    if (this.curToken !== ')') {
        throw new ParseError('expected \')\' at pos: ' + this.getTokenPos() + ', but found \'' + this.curToken + '\'');
    }
    if (operation.prototype.getArity() !== 0 && operation.prototype.getArity() !== stack.length) {
        throw new ParseError('invalid operation arguments at pos: ' + this.getTokenPos() +
            ' (expected ' + operation.prototype.getArity() + ' arguments, but found ' +  stack.length + ' arguments)');
    }
    return new operation(...stack);
}

Parser.prototype.parse = function () {
    if (this.curToken === '(') {
        this.nextToken();
        return this.parseBracket();
    } else if (this.curToken in varNameToIndex) {
        return new Variable(this.curToken)
    } else if (isNumber(this.curToken)) {
        return new Const(parseInt(this.curToken));
    } else {
        throw new ParseError('expected variable or number at pos: ' + this.getTokenPos() +
            ', but found: \'' + this.curToken + '\'');
    }
};

const parseWithBrackets = parseBracket => (str) => {
    str = str.trim();
    if (str.length === 0) {
        throw new ParseError('empty expression');
    }
    let parser = new Parser(str, parseBracket);
    parser.nextToken();
    let res = parser.parse();
    if (parser.curPos < parser.expression.length) {
        throw new ParseError('expected end of expression at pos: ' + parser.getTokenPos());
    }
    return res;
};

const parsePrefix = parseWithBrackets(
    function () {
        let operation = this.getOperation();
        let stack = this.parseOperationArgs();
        return this.checkAndOfBracket(operation, stack);
    });

const parsePostfix = parseWithBrackets(
    function () {
        let stack = this.parseOperationArgs();
        let operation = this.getOperation();
        return this.checkAndOfBracket(operation, stack);
    });