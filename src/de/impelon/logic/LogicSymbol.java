package de.impelon.logic;

/**
 * <p> Enumeration for referencing logic operators. </p>
 * 
 * @author Impelon
 */
public enum LogicSymbol {
	
	TRUE("true", 't', "\\true"),
	FALSE("false", 'f', "\\false"),
	VERUM("T", 'T', "\\top"),
	FALSUM("F", 'F', "\\bot"),
	NOT("~", '-', "\\neg"),
	AND("&&", '*', "\\wedge"),
	OR("||", '+', "\\vee"),
	BIIMPLICATION("<->", '=', "\\leftrightarrow"),
	IMPLICATION_RIGHT("->", '}', "\\to"),
	IMPLICATION_LEFT("<-", '{', "\\gets"),
	PARENTHESIS_RIGHT(")", ')', ")"),
	PARENTHESIS_LEFT("(", '(', "(");
	
	private final String readableNotation;
	private final char symbol;
	private final String latexNotation;
	
	LogicSymbol(String readableNotation, char symbol, String latexNotation) {
		this.readableNotation = readableNotation;
		this.symbol = symbol;
		this.latexNotation = latexNotation;
	}
	
	public char getSymbol() {
		return this.symbol;
	}
	
	public String getReadableNotation() {
		return this.readableNotation;
	}
	
	public String getLaTeXNotation() {
		return this.latexNotation;
	}
	
}
