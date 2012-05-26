package lisp;

import java.io.FileNotFoundException;

public class Compiler {
	private static boolean parseArguments(String [] args){
		if(args.length < 1){
			System.out.println("No input file");
			return false;
		}
		return true;
	}
	
	public static void main(String [] args){	
		FileTokenizer tokenizer = new FileTokenizer();
		if(parseArguments(args)){
			try {
				tokenizer.loadFile(args[0]);
			} catch (FileNotFoundException e) {
				e.getMessage();
			}
			Parser parser = new Parser();
			parser.compile(tokenizer);
		}
	}
}
