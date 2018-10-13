package de.impelon.minilogiceva;

import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import de.impelon.logic.LogicFormula;
import de.impelon.logic.LogicParser;
import de.impelon.logic.LogicSymbol;

public class MiniLogicEvaMain {
	
	protected static JFrame frame = new JFrame("MiniLogic: minimal logical evaluation");
	protected static JTextField formulaInput = new JTextField("(~(r -> q) -> (s <-> ~q)) || ~(r || s)");
	protected static JButton confirmationButton = new JButton("confirm");
	protected static LogicParser parser = new LogicParser();

	public static void main(String[] args) {
		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS));
		
		panel.add(new JLabel("Input logic expression here:"));
		panel.add(formulaInput);
		formulaInput.setToolTipText("<HTML>"
				+ "<p>Syntax:</p>"
				+ "<p>Single characters can be used as placeholders for variables.</p>"
				+ "<p>Parantheses can be used inside expressions.</p>"
				+ "<p>Use '+' or '&&' to indicate an AND-operator.</p>"
				+ "<p>Use '*' or '||' to indicate an OR-operator.</p>"
				+ "<p>Use '-' or '~' to indicate an NOT-operator.</p>"
				+ "<p>Use '->' to indicate an implication.</p>"
				+ "<p>Use '<-' to indicate a reverse implication.</p>"
				+ "<p>Use '<->' to indicate a biimplication or logical equivalence.</p>"
				+ "<p>Use 'T' to indicate Verum.</p>"
				+ "<p>Use 'F' to indicate Falsum.</p>"
				+ "</HTML>");
		formulaInput.setFont(new Font(Font.MONOSPACED, Font.BOLD, 16));
		panel.add(new JLabel("Result can be found in 'output.txt' after confirmation"));
		panel.add(confirmationButton);
		confirmationButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				finish();
			}
		});
		
		frame.add(panel);
		frame.setResizable(false);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	}
	
	public static void finish() {
		PrintWriter writer;
		try {
			writer = new PrintWriter("output.txt", "UTF-8");
		} catch (FileNotFoundException | UnsupportedEncodingException e) {
			e.printStackTrace();
			System.exit(0);
			return;
		}
		LogicFormula formula = new LogicFormula(formulaInput.getText());
		
		writer.println("Given expression:");
		writer.println(formulaInput.getText());
		try {
			formula.isUniversal();
		} catch (Exception ex) {
			writer.println("An exception occured. Probably the given expression is not a valid logical expression!");
			writer.println("");
			writer.println("========================");
			writer.println("");
			ex.printStackTrace(writer);
			writer.close();
			System.exit(0);
		}
		writer.println("");
		writer.println("-------Notations--------");
		writer.println("");
		writer.println("Compact notation (internal): " + formula.toString());
		writer.println("Readable notation: " + formula.toReadableNotation());
		writer.println("LaTeX notation: " + formula.toLaTeXNotation());
		writer.println("");
		writer.println("-------Properties-------");
		writer.println("");
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
		
		
		LogicSymbol[][] rowData = getRowData(formula);
		
		writer.println("");
		writer.println("------Truth Table-------");
		writer.println("");
		int i = 0;
		int delimit = formula.getLogicVariables().length;
		for(String s : getColumnNamesReadable(formula)) {
			if (i == delimit)
				writer.append("|\t");
			writer.append(s + "\t");
			i++;
		}
		writer.println("");
		for (LogicSymbol[] row : rowData) {
			i = 0;
			for (LogicSymbol cell : row) {
				if (i == delimit)
					writer.append("|\t");
				writer.print(cell.getSymbol() + "\t");
				i++;
			}
			writer.println("");
		}
		
		
		writer.println("");
		writer.println("---LaTeX Truth Table----");
		writer.println("");
		
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
		
		for (LogicSymbol[] row : rowData) {
			for (i = 0; i < row.length; i++) {
				writer.print(row[i].getLaTeXNotation());
				if (i < row.length - 1)
					writer.append("\t\t&");
			}
			writer.println("\\\\");
		}
		
		writer.println("\\end{array}");
		
		writer.close();
		System.exit(0);
	}
	
	public static int getColumnLength(LogicFormula formula) {
		return formula.toString().replace(LogicSymbol.PARENTHESIS_LEFT.getReadableNotation(), "")
				.replace(LogicSymbol.PARENTHESIS_RIGHT.getReadableNotation(), "").length() + 
				formula.getLogicVariables().length;
	}
	
	public static String[] getColumnNamesReadable(LogicFormula formula) {
		String[] columnNames = new String[getColumnLength(formula)];
		int index = 0;
		
		for (char variable : formula.getLogicVariables())
			columnNames[index++] = String.valueOf(variable);
		
		for (String s : formula.toReadableNotation().split(" "))
			columnNames[index++] = s;
				
		return columnNames;
	}
	
	public static String[] getColumnNamesLaTeX(LogicFormula formula) {
		String[] columnNames = new String[getColumnLength(formula)];
		int index = 0;
		
		for (char variable : formula.getLogicVariables())
			columnNames[index++] = String.valueOf(variable);
		
		for (String s : formula.toLaTeXNotation().split(" "))
			columnNames[index++] = s;
				
		return columnNames;
	}
	
	public static LogicSymbol[][] getRowData(LogicFormula formula) {
		List<List<LogicSymbol>> assignments = formula.getPossibleVariableAssignments();
		LogicSymbol[][] rowData = new LogicSymbol[assignments.size()][getColumnLength(formula)];
		
		for (int n = 0; n < rowData.length; n++) {
			int index = 0;
			for (LogicSymbol variable : assignments.get(n))
				rowData[n][index++] = parser.parse(String.valueOf(variable.getSymbol()));
			
			for (LogicSymbol result : parser.getPartialResults(formula.getPossibleStatements().get(n).toString())) {
				rowData[n][index++] = result;
			}
		}
		
		return rowData;
	}

}
