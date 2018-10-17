package de.impelon.logic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * <p> Class for representing logic expressions. </p>
 * 
 * @author Impelon
 */
public class LogicFormula {
	
	protected String formula;
	protected LogicSymbol result = null;
	protected static LogicParser parser = new LogicParser();
	
	public LogicFormula(String formula) {
		this.formula = formula;
		this.normalize();
	}
	
	protected void normalize() {
		for (LogicSymbol symbol : LogicSymbol.values())
			this.formula = this.formula.replace(symbol.getReadableNotation(), String.valueOf(symbol.getSymbol()))
				.replace(symbol.getLaTeXNotation(), String.valueOf(symbol.getSymbol()));
		this.formula = this.formula.replace(" ", "").replace(LogicSymbol.FALSE.getSymbol(), LogicSymbol.FALSUM.getSymbol())
				.replace(LogicSymbol.TRUE.getSymbol(), LogicSymbol.VERUM.getSymbol());
	}
	
	@Override
	public String toString() {
		return this.formula;
	}
	
	public String toReadableNotation() {
		String readableNotation = this.formula.replace("", " ").trim().replace("( ", "(").replace(" )", ")");
		
		for (LogicSymbol symbol : LogicSymbol.values())
			readableNotation = readableNotation.replace(String.valueOf(symbol.getSymbol()), symbol.getReadableNotation());
		return readableNotation;
	}
	
	public String toLaTeXNotation() {
		String latexNotation = this.formula.replace("", " ").trim().replace("( ", "(").replace(" )", ")");		
		
		for (LogicSymbol symbol : LogicSymbol.values())
			latexNotation = latexNotation.replace(String.valueOf(symbol.getSymbol()), symbol.getLaTeXNotation());
		return latexNotation;
	}
	
	public char[] getLogicVariables() {
		String formula = this.formula;
		for (LogicSymbol symbol : LogicSymbol.values())
			formula = formula.replace(String.valueOf(symbol.getSymbol()), "");
		
		String variables = "";
		for (char c : formula.toCharArray())
			if (!variables.contains(String.valueOf(c)))
				variables += c;
		
		char[] array = variables.toCharArray();
		Arrays.sort(array);
		return array;
	}
	
	public List<List<LogicSymbol>> getPossibleVariableAssignments() {
		char[] variables = this.getLogicVariables();
		
		List<List<LogicSymbol>> variableList = new ArrayList<List<LogicSymbol>>(variables.length);
		for (int n = 0; n < variables.length; n++) {
			List<LogicSymbol> variable = new ArrayList<LogicSymbol>(2);
			variable.add(LogicSymbol.FALSUM);
			variable.add(LogicSymbol.VERUM);
			variableList.add(variable);
		}
		return getCartesianProduct(variableList);
	}
	
	public List<LogicFormula> getPossibleStatements() {
		char[] variables = this.getLogicVariables();
		List<List<LogicSymbol>> assignments = this.getPossibleVariableAssignments();
		
		List<LogicFormula> statements = new ArrayList<LogicFormula>(assignments.size());
		for (List<LogicSymbol> assignment : assignments) {
			String statement = this.formula;
			for (int n = 0; n < variables.length; n++)
				statement = statement.replace(variables[n], assignment.get(n).getSymbol());
			statements.add(new LogicFormula(statement));
		}
		
		return statements;
	}
	
	public boolean isStatement() {
		return this.getLogicVariables().length == 0;
	}
	
	public boolean isPattern() {
		return !this.isStatement();
	}
	
	public boolean isAtomar() {
		return this.formula.length() == 1;
	}
	
	public boolean isCompound() {
		return !this.isAtomar();
	}
	
	public boolean isTrivial() {
		return this.formula.length() <= 3;
	}
	
	public boolean isUniversal() {
		return this.isEquivalent(new LogicFormula(LogicSymbol.VERUM.getReadableNotation()));
	}
	
	public boolean isContradictory() {
		return this.isEquivalent(new LogicFormula(LogicSymbol.FALSUM.getReadableNotation()));
	}
	
	public boolean isSatisfiable() {
		return !this.isContradictory();
	}
	
	public boolean isEquivalent(LogicFormula comparison) {
		List<LogicFormula> statements = this.getPossibleStatements();
		List<LogicFormula> comparisonStatements = comparison.getPossibleStatements();
		
		while (statements.size() < comparisonStatements.size())
			statements.addAll(statements);
		while (comparisonStatements.size() < statements.size())
			comparisonStatements.addAll(comparisonStatements);
				
		for (int n = 0; n < statements.size(); n++)
			if (statements.get(n).evaluateBoolean() != comparisonStatements.get(n).evaluateBoolean())
				return false;
		return true;	
	}
	
	/**
	 * <p> Evaluates the LogicFormula and returns the result as a LogicSymbol. </p>
	 * 
	 * @return either LogicSymbol.TRUE or LogicSymbol.FALSE or null if the LogicFormula could not be evaluated
	 */
	public LogicSymbol evaluate() {
		if (this.result == null) {
			if (this.getLogicVariables().length != 0)
				return null;
			this.result = parser.parse(this.formula);
		}
		return this.result;
	}
	
	/**
	 * <p> Evaluates the LogicFormula and returns the result as a boolean. </p>
	 * 
	 * @return whether the expression evaluates to true
	 */
	public boolean evaluateBoolean() {
		return this.evaluate().equals(LogicSymbol.TRUE);
	}
	
	/**
	 * <p> Calculates and returns the cartesian product from the given lists. </p>
	 * 
	 * <p> Modified Version of: https://stackoverflow.com/a/9496234 </p>
	 * 
	 * @param lists list of multiple lists of elements to build a Cartesian product from
	 * @author original author: Philipp Meister (https://stackoverflow.com/users/1239812/philipp-meister)
	 * @return the cartesian product
	 */
	public static <T> List<List<T>> getCartesianProduct(List<List<T>> lists) {
	    List<List<T>> result = new ArrayList<List<T>>();
	    if (lists.isEmpty()) {
	        result.add(new ArrayList<T>());
	        return result;
	    }
        List<T> firstList = lists.get(0);
        List<List<T>> remainingLists = getCartesianProduct(lists.subList(1, lists.size()));
        for (T element : firstList) {
            for (List<T> remainingList : remainingLists) {
                ArrayList<T> partialResult = new ArrayList<T>();
                partialResult.add(element);
                partialResult.addAll(remainingList);
                result.add(partialResult);
            }
        }
	    return result;
	}

}
