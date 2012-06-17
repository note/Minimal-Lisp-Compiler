package lisp;

import java.util.ArrayList;
import java.util.HashMap;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;


public class Symbol extends LispForm{
	public String name;
	
	public Symbol(String name){
		this.name = name;
	}
	
	public String getName(){
		return name;
	}
	
	protected String generateMethodDescriptor(int paramsNum){
		String methodDesc = "(";
		for(int i=0; i<paramsNum; ++i)
			methodDesc += "Llisp/LispForm;";
		methodDesc += ")Llisp/LispForm;";
		return methodDesc;
	}

	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {

	}
	
	protected java.util.List<LispForm> getParameters() throws SyntaxException{
		if(!(parent instanceof List))
			throw new SyntaxException("Function invocation not inside parenthesis");
		
		return ((List) parent).getParameters();
	}
	
	public String toString(){
		return getName();
	}

	@Override
	public void generateYourself(SymbolTable symbolTable)
			throws SyntaxException {
		MethodVisitor mv = Factory.getMethodVisitor();
		mv.visitTypeInsn(Opcodes.NEW, "lisp/Symbol");
		mv.visitInsn(Opcodes.DUP);
		mv.visitLdcInsn(getName());
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/Symbol", "<init>", "(Ljava/lang/String;)V");
	}
	
	protected java.util.List<LispForm> wrapWithPrognStartingFromIndex(int index) throws SyntaxException{
		java.util.List<LispForm> parameters = getParameters(); 
		java.util.List<LispForm> res = new ArrayList<LispForm>();
		res.add(parameters.get(0));
		res.add(parameters.get(1));
		
		List progn = List.createForm("progn");
		for(int i=index; i<parameters.size(); ++i) //i=index, we omit previous arguments
			progn.addChild(parameters.get(i));
		res.add(progn);
		return res;
	}
	
	protected HashMap<String, Integer> createAddrMap(List list, String operatorName) throws SyntaxException{
		HashMap<String, Integer> res = new HashMap<String, Integer>();
		
		/*
		 * A few assumptions here:
		 * - all arguments must be one-word size.
		 * - consecutive arguments are numbered with next negative indexes starting from -1 (caller must follow this convention)
		 */
		int index = -1;
		for(LispForm it : list.getChildren()){
			if(!(it instanceof Symbol))
				throw new SyntaxException("Elements of " + operatorName + " arguments list are expected to be symbols.");
			
			res.put(((Symbol) it).getName(), index);
			--index;
		}
		
		return res;
	}
}
