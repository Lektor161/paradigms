"use strict";

const varNameToIndex = {
	'x': 0,
	'y': 1,
	'z': 2
};

// :NOTE: Эффектиность
const variable = (varName) => (...args) => args[varNameToIndex[varName]];

const cnst = (val) => () => val;
const pi = cnst(Math.PI);
const e = cnst(Math.E);

const makeExpression = (calcFunction) => (...expressions) => (...args) => {
	return calcFunction(...expressions.map(expression => expression(...args)));
};

const add = makeExpression((a, b) => a + b);
const subtract = makeExpression((a, b) => a - b);
const multiply = makeExpression((a, b) => a * b);
const divide = makeExpression((a, b) => a / b);
const negate = makeExpression((a) => -a);

const avg5 = makeExpression(
	(...args) => {
		// :NOTE: Array.?
		let sum = 0;
		for (let arg of args) {
			sum += arg;
		}
		return sum / args.length;
	}
);

const med3 = makeExpression(
	(...args) => args.sort((a, b) => a - b) [(args.length - 1) / 2]
);

const opers = [ 'negate', '+', '-', '*', '/', 'med3', 'avg5' ];
const varNames = [ 'x', 'y', 'z' ];
const cnstNames = [ 'pi', 'e' ];

const tokenToArity = {
	'negate': 1,
	'+': 2,
	'-': 2,
	'*': 2,
	'/': 2,
	'med3': 3,
	'avg5': 5
};

const tokenToOperation = {
	'negate': negate,
	'+': add,
	'-': subtract,
	'*': multiply,
	'/': divide,
	'med3': med3,
	'avg5': avg5
};

const tokenToCnst = {
	'pi': pi,
	'e': e
};

const parse = (str) => {
	let stack = [];
	let tokens = str.split(' ').filter((word) => word !== '');
	for (let token of  tokens) {
		if (opers.includes(token)) {
			let arity = tokenToArity[token];
			let arr = stack.splice(-arity);
			stack.push(tokenToOperation[token](...arr));
		} else if (varNames.includes(token)) {
			stack.push(variable(token));
		} else if (cnstNames.includes(token)) {
			stack.push(tokenToCnst[token]);
		} else {
			stack.push(cnst(Number.parseInt(token)));
		}
	}

	return stack.pop();
};
