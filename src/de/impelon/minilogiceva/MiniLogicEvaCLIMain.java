package de.impelon.minilogiceva;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;

public class MiniLogicEvaCLIMain {
	
	public static void main(String[] args) {
		
		if (args.length <= 0) {
			System.out.println("No arguments given! Try launching this with '--help'.");
			return;
		}
		
		PrintWriter writer = null;
		String formula = null;
		
		for (String arg : args) {
			if (arg.equals("--help") || arg.equals("-h")) {
				System.out.println("MiniLogicEva accepts logic expressions as arguments and evaluates them.");
				System.out.println("To pass an expression as argument surround it with quotation marks e.g. 'x -> y'.");
				System.out.println();
				System.out.println("Syntax:");
				System.out.println("Single characters can be used as placeholders for variables.");
				System.out.println("Parantheses can be used inside expressions.");
				System.out.println("Use '+' or '&&' to indicate an AND-operator.");
				System.out.println("Use '*' or '||' to indicate an OR-operator.");
				System.out.println("Use '-' or '~' to indicaqte an NOT-operator.");
				System.out.println("Use '->' to indicate an implication.");
				System.out.println("Use '<-' to indicate a reverse implication.");
				System.out.println("Use '<->' to indicate a biimplication or logical equivalence.");
				System.out.println("Use 'T' or 't' to indicate Verum.");
				System.out.println("Use 'F' or 'f' to indicate Falsum.");
				System.out.println();
				System.out.println("Options:");
				System.out.println("--help, -h \t provides a help message.");
				System.out.println("--print, -p \t prints any output to the standard output instead of writing to 'output.txt'.");
				return;
			} else if (arg.equals("--print") || arg.equals("-p")) {
				writer = new PrintWriter(System.out);
			} else {
				formula = arg;
			}
		}
		
		if (formula == null) {
			System.out.println("No expression given! Try launching this with '--help'.");
			return;
		}
		
		if (writer == null) {
			try {
				writer = new PrintWriter("output.txt", "UTF-8");
			} catch (FileNotFoundException | UnsupportedEncodingException ex) {
				ex.printStackTrace();
				return;
			}
		}
		
		MiniLogicEvaCore.printOutput(formula, writer);
		
		writer.close();
	}

}
