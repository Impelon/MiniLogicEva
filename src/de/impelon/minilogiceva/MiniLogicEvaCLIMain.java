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
		boolean skipHeaders = false;
		boolean skipNotations = false;
		boolean skipProperties = false;
		boolean skipReadableTable = false;
		boolean skipLaTeXTable = false;
		boolean simpleTables = false;
		
		for (String arg : args) {
			switch(arg) {
				case "--help":
				case "-h":
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
					System.out.println("--help, -h \t\t provides a help message.");
					System.out.println("--version, -v \t\t prints this implementation's version.");
					System.out.println("--checkVersion \t checks for the latest version.");
					System.out.println("--print, -p \t\t prints any output to the standard output instead of writing to 'output.txt'.");
					System.out.println("--skipHeaders \t\t skips headers when printing.");
					System.out.println("--skipNotations \t skips notations when printing.");
					System.out.println("--skipProperties \t skips properties when printing.");
					System.out.println("--skipReadableTable \t skips the readable truth table when printing.");
					System.out.println("--skipLaTeXTable \t skips the LaTeX truth table when printing.");
					System.out.println("--simpleTables \t only writes the end result of each row into truth tables.");
					return;
				case "--version":
				case "-v":
					System.out.println("MiniLogicEvaCLI - version: " + MiniLogicEvaCore.VERSION);
					return;
				case "--checkVersion":
					String url = MiniLogicEvaCore.getUpdateWebsite();
					if (url == null) {
						System.out.println("Up to date!");
					} else {
						System.out.println("New version available at: " + url);
					}
					return;
				case "--print":
				case "-p":
					writer = new PrintWriter(System.out);
					break;
				case "--skipHeaders":
					skipHeaders = true;
					break;
				case "--skipNotations":
					skipNotations = true;
					break;
				case "--skipProperties":
					skipProperties = true;
					break;
				case "--skipReadableTable":
					skipReadableTable = true;
					break;
				case "--skipLaTeXTable":
					skipLaTeXTable = true;
					break;
				default:
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
		
		MiniLogicEvaCore.printOutput(formula, writer, skipHeaders, skipNotations, skipProperties, skipReadableTable, skipLaTeXTable, simpleTables);
		
		writer.close();
	}

}
