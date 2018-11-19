package de.impelon.minilogiceva;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Writer;
import java.net.URL;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
	public final static String VERSION = new BufferedReader(new InputStreamReader(MiniLogicEvaCore.class.getResourceAsStream("/info.txt"))).lines()
			.filter(x -> x.startsWith("version: ")).collect(Collectors.toList()).get(0).replace("version:", "").trim();
	public final static String WEBSITE = new BufferedReader(new InputStreamReader(MiniLogicEvaCore.class.getResourceAsStream("/info.txt"))).lines()
			.filter(x -> x.startsWith("website: ")).collect(Collectors.toList()).get(0).replace("website:", "").trim();
	
	/**
	 * <p> Returns the remote info-file as a Stream. </p>
	 * 
	 * @return the Stream of the info-file; null if any exception occurred
	 */
	protected static Stream<String> getRemoteInfo() {
		try {
			return new BufferedReader(new InputStreamReader(new URL("https://raw.githubusercontent.com/Impelon/MiniLogicEva/master/src/de/impelon/info.yml").openStream(), "UTF-8")).lines();
		} catch (Exception ex) {
			return null;
		}
	}
	
	/**
	 * <p> Returns the URL of the website of the updated remote version. </p>
	 * 
	 * @return URL of the update-website; null if the remote version is the same
	 */
	public static String getUpdateWebsite() {
		if (!isRemoteVersion()) {
			Stream<String> info = getRemoteInfo();
			if (info == null)
				return null;
			
			return info.filter(x -> x.startsWith("website: ")).collect(Collectors.toList()).get(0).replace("website:", "").trim();
		} else {
			return null;
		}
	}
	
	/**
	 * <p> Checks whether the current version is the same as the remote version. </p>
	 * 
	 * @return whether the current version is the same as the remote version
	 */
	public static boolean isRemoteVersion() {
		Stream<String> info = getRemoteInfo();
		if (info == null)
			return true;
		
		return info.filter(x -> x.startsWith("version: ")).collect(Collectors.toList()).get(0).replace("version:", "").trim().equals(VERSION);
	}
	
	/**
	 * <p> Prints all information about the given input to the given Writer. </p>
	 * <p> The input will be interpreted as a LogicFormula. Otherwise this prints a stacktrace to the Writer. </p>
	 * <p> This will <b><i>not</i></b> close the writer! </p>
	 * 
	 * @param input String with logic expression to output information about
	 * @param originalwriter Writer to write to
	 */
	public static void printOutput(String input, Writer originalwriter) {
		printOutput(input, originalwriter, false, false, false, false, false, false);
	}
	
	/**
	 * <p> Prints information about the given input to the given Writer. </p>
	 * <p> The input will be interpreted as a LogicFormula. Otherwise this prints a stacktrace to the Writer. </p>
	 * <p> This will <b><i>not</i></b> close the writer! </p>
	 * 
	 * @param input String with logic expression to output information about
	 * @param originalwriter Writer to write to
	 * @param skipHeaders whether to skip headers when printing
	 * @param skipNotations whether to skip information about notations when printing
	 * @param skipProperties whether to skip properties when printing
	 * @param skipReadableTable whether to skip the readable truth table when printing
	 * @param skipLaTeXTable whether to skip LaTeX truth table when printing
	 * @param simpleTable whether to put only the end result of each row into the table
	 */
	public static void printOutput(String input, Writer originalwriter, 
			boolean skipHeaders, boolean skipNotations, boolean skipProperties, boolean skipReadableTable, boolean skipLaTeXTable, boolean simpleTables) {
		PrintWriter writer = new PrintWriter(originalwriter);
		LogicFormula formula = new LogicFormula(input);
		if (!skipHeaders) {
			writer.println("Given input:");
			writer.println(input);	
		}
		
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
		
		if (!skipNotations) {
			if (!skipHeaders) {
				writer.println();
				writer.println("-------Notations--------");
				writer.println();	
			}
			writer.println("Compact notation (internal): " + formula.toString());
			writer.println("Readable notation: " + formula.toReadableNotation());
			writer.println("LaTeX notation: " + formula.toLaTeXNotation());
		}
		
		if (!skipProperties) {
			if (!skipHeaders) {
				writer.println();
				writer.println("-------Properties-------");
				writer.println();
			}
			writer.println("Is statement (aka. has no variables): ");
			writer.println("  " + String.valueOf(formula.isStatement()).toUpperCase());
			writer.println("Is pattern (aka. has variables): ");
			writer.println("  " + String.valueOf(formula.isPattern()).toUpperCase());
			writer.println("Is atomic (aka. single operand): ");
			writer.println("  " + String.valueOf(formula.isAtomic()).toUpperCase());
			writer.println("Is compound (aka. multiple operands): ");
			writer.println("  " + String.valueOf(formula.isCompound()).toUpperCase());
			writer.println("Is trivial (aka. one operator and its operands): ");
			writer.println("  " + String.valueOf(formula.isTrivial()).toUpperCase());
			writer.println("Is universal (aka. true regardless of chosen variables): ");
			writer.println("  " + String.valueOf(formula.isUniversal()).toUpperCase());
			writer.println("Is contradictory (aka. false regardless of chosen variables): ");
			writer.println("  " + String.valueOf(formula.isContradictory()).toUpperCase());
			writer.println("Is satisfiable (aka. there is at least a choice of variables so that the expression evaluates to true): ");
			writer.println("  " + String.valueOf(formula.isSatisfiable()).toUpperCase());
		}
		
		if (!skipReadableTable) {
			if (!skipHeaders) {
				writer.println();
				writer.println("------Truth Table-------");
				writer.println();
			}
			printReadableTruthTable(formula, writer, simpleTables);
		}
		
		if (!skipLaTeXTable) {
			if (!skipHeaders) {
				writer.println();
				writer.println("---LaTeX Truth Table----");
				writer.println();
			}
			printLaTeXTruthTable(formula, writer, simpleTables);
		}		
	}
	
	/**
	 * <p> Prints the readable truth table of the given LogicFormula to the given PrintWriter. </p>
	 * 
	 * @param formula LogicFormula used to create the truth table
	 * @param simpleTable whether to put only the end result of each row into the table
	 * @param writer PrintWriter to write to
	 */
	public static void printReadableTruthTable(LogicFormula formula, PrintWriter writer, boolean simpleTable) {
		LogicSymbol[][] cellData = getCellData(formula, simpleTable);
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
	}
	
	/**
	 * <p> Prints the LaTeX truth table of the given LogicFormula to the given PrintWriter. </p>
	 * 
	 * @param formula LogicFormula used to create the truth table
	 * @param simpleTable whether to put only the end result of each row into the table
	 * @param writer PrintWriter to write to
	 */
	public static void printLaTeXTruthTable(LogicFormula formula, PrintWriter writer, boolean simpleTable) {
		writer.append("\\begin{array}{");
		
		String[] latexColunNames = getColumnNamesLaTeX(formula);
		LogicSymbol[][] cellData = getCellData(formula, simpleTable);
		int delimit = formula.getLogicVariables().length;
		int i = 0;
		
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
	 * @param simpleData whether to put only the end result of each row into the data
	 * @return an 2D-array containing LogicSymbols for each cell. Each 1D-array contains a row.
	 */
	public static LogicSymbol[][] getCellData(LogicFormula formula, boolean simpleData) {
		List<List<LogicSymbol>> assignments = formula.getPossibleVariableAssignments();
		LogicSymbol[][] cellData = new LogicSymbol[assignments.size()][getColumnAmount(formula)];
		
		for (int n = 0; n < cellData.length; n++) {
			int index = 0;
			for (LogicSymbol variable : assignments.get(n))
				cellData[n][index++] = parser.parse(String.valueOf(variable.getSymbol()));
			
			if (simpleData) {
				cellData[n][index++] = formula.getPossibleStatements().get(n).evaluate();
			} else {
				for (LogicSymbol result : parser.getPartialResults(formula.getPossibleStatements().get(n).toString())) {
					cellData[n][index++] = result;
				}
			}
			
		}
		
		return cellData;
	}

}
