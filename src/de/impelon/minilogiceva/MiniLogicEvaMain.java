package de.impelon.minilogiceva;

import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class MiniLogicEvaMain {
	
	protected static JFrame frame = new JFrame("MiniLogicEva 1.1: minimalistic logic expression evaluation");
	protected static JTextField formulaInput = new JTextField("(~(r -> q) -> (s <-> ~q)) || ~(r || s)");
	protected static JButton confirmationButton = new JButton("confirm");

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
				+ "<p>Use 'T' or 't' to indicate Verum.</p>"
				+ "<p>Use 'F' or 'f' to indicate Falsum.</p>"
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
		} catch (FileNotFoundException | UnsupportedEncodingException ex) {
			ex.printStackTrace();
			System.exit(0);
			return;
		}
		
		MiniLogicEvaCore.printOutput(formulaInput.getText(), writer);
		
		writer.close();
		System.exit(0);
	}

}
