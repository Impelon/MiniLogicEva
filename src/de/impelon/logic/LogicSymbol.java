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
	
	/**
	 * <p> Returns a single character representing the LogicSymbol. </p>
	 * 
	 * @return the symbol representation
	 */
	public char getSymbol() {
		return this.symbol;
	}
	
	/**
	 * <p> Returns a String representing the LogicSymbol in a readable fashion. </p>
	 * 
	 * @return the String representation
	 */
	public String getReadableNotation() {
		return this.readableNotation;
	}
	
	/**
	 * <p> Returns a String representing the LogicSymbol in LaTeX. </p>
	 * 
	 * @return the LaTeX representation
	 */
	public String getLaTeXNotation() {
		return this.latexNotation;
	}
	
}
