package de.impelon.minilogiceva;

import java.io.PrintWriter;
import java.io.Writer;
import java.util.List;

import de.impelon.logic.LogicFormula;
import de.impelon.logic.LogicParser;
import de.impelon.logic.LogicSymbol;

/**
 * <p> Core class for the MiniLogicEva-Applications. </p>
 * 
 * @author Impelon
 */

public class MiniLogicEvaCore {
	
	protected static LogicParser parser = new LogicParser();
	
	public static void printOutput(String input, Writer originalwriter) {
		PrintWriter writer = new PrintWriter(originalwriter);
		LogicFormula formula = new LogicFormula(input);
		writer.println("Given input:");
		writer.println(input);
		try {
			formula.isUniversal();
		} catch (Exception ex) {
			writer.println("An exception occured. Probably the given expression is not a valid logical expression!");
			writer.println();
			writer.println("========================");
			writer.println();
			ex.printStackTrace(writer);
			writer.close();
			return;
		}
		writer.println();
		writer.println("-------Notations--------");
		writer.println();
		writer.println("Compact notation (internal): " + formula.toString());
		writer.println("Readable notation: " + formula.toReadableNotation());
		writer.println("LaTeX notation: " + formula.toLaTeXNotation());
		writer.println();
		writer.println("-------Properties-------");
		writer.println();
		writer.println("Is statement (aka. has no variables): ");
		writer.println("  " + String.valueOf(formula.isStatement()).toUpperCase());
		writer.println("Is pattern (aka. has variables): ");
		writer.println("  " + String.valueOf(formula.isPattern()).toUpperCase());
		writer.println("Is atomar (aka. single operand): ");
		writer.println("  " + String.valueOf(formula.isAtomar()).toUpperCase());
		writer.println("Is compound (aka. multiple operands): ");
		writer.println("  " + String.valueOf(formula.isCompound()).toUpperCase());
		writer.println("Is trivial (aka. one operator and its operands): ");
		writer.println("  " + String.valueOf(formula.isTrivial()).toUpperCase());
		writer.println("Is universal (aka. true regardless of chosen variables): ");
		writer.println("  " + String.valueOf(formula.isUniversal()).toUpperCase());
		writer.println("Is contradictory (aka. false regardless of chosen variables): ");
		writer.println("  " + String.valueOf(formula.isContradictory()).toUpperCase());
		writer.println("Is satisfiable (aka. there is atleast a choice of variables so that the expression evaluates to true): ");
		writer.println("  " + String.valueOf(formula.isSatisfiable()).toUpperCase());
		
		
		LogicSymbol[][] cellData = getCellData(formula);
		
		writer.println();
		writer.println("------Truth Table-------");
		writer.println();
		int i = 0;
		int delimit = formula.getLogicVariables().length;
		for(String s : getColumnNamesReadable(formula)) {
			if (i == delimit)
				writer.append("|\t");
			writer.append(s + "\t");
			i++;
		}
		writer.println();
		for (LogicSymbol[] row : cellData) {
			i = 0;
			for (LogicSymbol cell : row) {
				if (i == delimit)
					writer.append("|\t");
				writer.print(cell.getSymbol() + "\t");
				i++;
			}
			writer.println();
		}
		
		
		writer.println();
		writer.println("---LaTeX Truth Table----");
		writer.println();
		
		writer.append("\\begin{array}{");
		
		String[] latexColunNames = getColumnNamesLaTeX(formula);
		
		for (i = 0; i < latexColunNames.length; i++) {
			if (i == delimit)
				writer.append("||");
			writer.append("c");
		}
		writer.println("}");
		
		for (i = 0; i < latexColunNames.length; i++) {
			writer.append(latexColunNames[i]);
			if (i < latexColunNames.length - 1)
				writer.append("\t\t&");
		}
		writer.println("\\\\");
		
		for (LogicSymbol[] row : cellData) {
			for (i = 0; i < row.length; i++) {
				writer.print(row[i].getLaTeXNotation());
				if (i < row.length - 1)
					writer.append("\t\t&");
			}
			writer.println("\\\\");
		}
		
		writer.println("\\end{array}");
		
		writer.close();
	}
	
	/**
	 * <p> Returns the number of columns for a truth table. </p>
	 * 
	 * @param formula the given LogicFormula to get the number of columns for
	 * @return the number of columns
	 */
	public static int getColumnAmount(LogicFormula formula) {
		return formula.toString().replace(LogicSymbol.PARENTHESIS_LEFT.getReadableNotation(), "")
				.replace(LogicSymbol.PARENTHESIS_RIGHT.getReadableNotation(), "").length() + 
				formula.getLogicVariables().length;
	}
	
	/**
	 * <p> Returns the column names of the truth table for a readable representation. </p>
	 * 
	 * @param formula the given LogicFormula to get the column names for
	 * @return the column names as an array
	 */
	public static String[] getColumnNamesReadable(LogicFormula formula) {
		String[] columnNames = new String[getColumnAmount(formula)];
		int index = 0;
		
		for (char variable : formula.getLogicVariables())
			columnNames[index++] = String.valueOf(variable);
		
		for (String s : formula.toReadableNotation().split(" "))
			columnNames[index++] = s;
				
		return columnNames;
	}
	
	/**
	 * <p> Returns the column names of the truth table for LaTeX representation. </p>
	 * 
	 * @param formula the given LogicFormula to get the column names for
	 * @return the column names as an array
	 */
	public static String[] getColumnNamesLaTeX(LogicFormula formula) {
		String[] columnNames = new String[getColumnAmount(formula)];
		int index = 0;
		
		for (char variable : formula.getLogicVariables())
			columnNames[index++] = String.valueOf(variable);
		
		for (String s : formula.toLaTeXNotation().split(" "))
			columnNames[index++] = s;
				
		return columnNames;
	}
	
	/**
	 * <p> Returns the contents of each cell for a truth table as LogicSymbols. </p>
	 * 
	 * @param formula the given LogicFormula to get the cells for
	 * @return an 2D-array containing LogicSymbols for each cell. Each 1D-array contains a row.
	 */
	public static LogicSymbol[][] getCellData(LogicFormula formula) {
		List<List<LogicSymbol>> assignments = formula.getPossibleVariableAssignments();
		LogicSymbol[][] cellData = new LogicSymbol[assignments.size()][getColumnAmount(formula)];
		
		for (int n = 0; n < cellData.length; n++) {
			int index = 0;
			for (LogicSymbol variable : assignments.get(n))
				cellData[n][index++] = parser.parse(String.valueOf(variable.getSymbol()));
			
			for (LogicSymbol result : parser.getPartialResults(formula.getPossibleStatements().get(n).toString())) {
				cellData[n][index++] = result;
			}
		}
		
		return cellData;
	}

}
