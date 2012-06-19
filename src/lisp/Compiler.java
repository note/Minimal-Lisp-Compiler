package lisp;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

public class Compiler {
	private String outputDir = ".";
	private String filename;
	
	private boolean parseArguments(String [] args){
		if(args.length < 1){
			System.out.println("No input file");
			return false;
		}else{
			boolean loadOutputDir = false;
			for(int i=0; i<args.length; ++i){
				if(loadOutputDir){
					outputDir = args[i];
				}else{
					if(args[i].equals("-o"))
						loadOutputDir = true;
					else
						filename = args[i];
				}
			}
			
			return true;
		}
	}
	
	private java.util.List<LispForm> addTopLevelToMain(java.util.List<LispForm> tree){
		java.util.List<LispForm> res = new ArrayList<LispForm>();
		
		List progn = List.createForm("progn");
		List mainDefun = List.createDefun("Main", List.createEmptyList(), progn);
		
		for(LispForm it : tree)
			if(it instanceof List && !((List) it).isDefun() && ! ((List) it).isMacro())
				progn.addChild(it);
			else
				res.add(it);
		
		if(progn.getParameters().size() > 0)
			res.add(mainDefun);
		return res;
	}
	
	private void compile(java.util.List<LispForm> tree) throws SyntaxException{
		tree = addTopLevelToMain(tree);
		
		SymbolTable st = new SymbolTable();
		for(LispForm it : tree){
			LispForm form = it.expandMacros(st);
			form.compile(st);
		}
	}
	
	private void run(String [] args){
		if(parseArguments(args)){
			try {
				FileTokenizer tokenizer = new FileTokenizer();
				tokenizer.loadFile(filename);
				Generator.setOutputDir(outputDir);
				Parser parser = new Parser();
				java.util.List<LispForm> tree = parser.parse(tokenizer);
				compile(tree);				
			} catch (FileNotFoundException e) {
				e.getMessage();
			} catch (SyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
	private void test() throws SyntaxException, IOException{
		Generator.setOutputDir("generated");
		Parser p = new Parser();
		Tokenizer tokenizer = new Tokenizer();
		tokenizer.loadInput("(do ((i 0 (+ 3 i))) ((> i 10) i) (print i))");
//		tokenizer.loadInput("(print (let ((fn (lambda (x) (* x x)))) (funcall fn 4)))");
		java.util.List<LispForm> tree = p.parse(tokenizer);
		compile(tree);
	}
	
	public static void main(String [] args){	
		Compiler compiler = new Compiler();
		compiler.run(args);
		
//		try {
//			compiler.test();
//		} catch (SyntaxException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
	}
}
