package de.impelon.logic;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * <p> Modified Version of: https://stackoverflow.com/a/26227947 </p>
 * 
 * <p> Parses a logic statement (logic formula without variables) and returns its result. </p>
 * 
 * @author Boann, Impelon
 */
public class LogicParser {
	
	protected int pos, ch;
	protected String equation;
	protected LinkedHashMap<Integer, LogicSymbol> partialResults = new LinkedHashMap<Integer, LogicSymbol>();
	
	void nextChar() {
		ch = (++pos < equation.length()) ? equation.charAt(pos) : -1;
	}

	boolean eat(int charToEat) {
		while (ch == ' ')
			nextChar();
		if (ch == charToEat) {
			nextChar();
			return true;
		}
		return false;
	}
	
	public List<LogicSymbol> getPartialResults(String equation) {
		return this.getPartialResults(equation, false);
	}
	
	public List<LogicSymbol> getPartialResults(String equation, boolean postordering) {
		this.parse(equation);
		if (postordering)
			return new ArrayList<LogicSymbol>(this.partialResults.values());
		return this.partialResults.entrySet().stream()
				.sorted(Map.Entry.comparingByKey()).map(Map.Entry::getValue).collect(Collectors.toList());
	}

	public LogicSymbol parse(String equation) {
		this.partialResults.clear();
		this.pos = -1;
		this.equation = equation;
		nextChar();
		LogicSymbol x = parseExpression();
		if (pos < equation.length())
			throw new RuntimeException("Unexpected: '" + (char) ch + "' at position: " + this.pos);
		return x;
	}

	// Grammar:
	// expression = term | expression '{' term | expression '}' term
	// term = factor | term '+' factor | term '*' factor | term '=' factor
	// factor = '-' factor | '(' expression ')' | boolean

	protected LogicSymbol parseExpression() {
		LogicSymbol x = parseTerm();
		if (eat(LogicSymbol.IMPLICATION_LEFT.getSymbol())) {
			int opPos = this.pos;
			LogicSymbol y = parseTerm();
			x = (y.equals(LogicSymbol.TRUE) ? x : LogicSymbol.TRUE); // implication (reverse)
			this.partialResults.put(opPos, x);
		} else if (eat(LogicSymbol.IMPLICATION_RIGHT.getSymbol())) {
			int opPos = this.pos;
			LogicSymbol y = parseTerm();
			x = (x.equals(LogicSymbol.TRUE) ? y : LogicSymbol.TRUE); // implication
			this.partialResults.put(opPos, x);
		} else if (eat(LogicSymbol.BIIMPLICATION.getSymbol())) {
			int opPos = this.pos;
			x = (parseTerm().equals(x) ? LogicSymbol.TRUE : LogicSymbol.FALSE); // biimplication
			this.partialResults.put(opPos, x);
		}
		return x;
	}

	protected LogicSymbol parseTerm() {
		LogicSymbol x = parseFactor();
		if (eat(LogicSymbol.AND.getSymbol())) {
			int opPos = this.pos;
			x = (parseFactor().equals(x) ? x : LogicSymbol.FALSE); // conjunction
			this.partialResults.put(opPos, x);
		} else if (eat(LogicSymbol.OR.getSymbol())) {
			int opPos = this.pos;
			x = (parseFactor().equals(x) ? x : LogicSymbol.TRUE);  // disjunction
			this.partialResults.put(opPos, x);
		}
		return x;
	}

	protected LogicSymbol parseFactor() {
		LogicSymbol x;
		if (eat(LogicSymbol.NOT.getSymbol())) {
			int opPos = this.pos;
			x = (parseFactor().equals(LogicSymbol.TRUE) ? LogicSymbol.FALSE : LogicSymbol.TRUE); // unary negation
			this.partialResults.put(opPos, x);
			return x;
		}
		
		if (eat(LogicSymbol.PARENTHESIS_LEFT.getSymbol())) { // parenthesis
			x = parseExpression();
			eat(LogicSymbol.PARENTHESIS_RIGHT.getSymbol());
		} else if (ch == LogicSymbol.FALSUM.getSymbol() || ch == LogicSymbol.VERUM.getSymbol()) {  	// booleans
			x = (ch == LogicSymbol.VERUM.getSymbol() ? LogicSymbol.TRUE : LogicSymbol.FALSE);
			nextChar();
			this.partialResults.put(this.pos, x);
		} else
			throw new RuntimeException("Unexpected: '" + (char) ch + "' at position: " + this.pos);

		return x;
	}
}
