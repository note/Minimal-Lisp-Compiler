package lisp.specialoperators;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;

import lisp.Factory;
import lisp.Generator;
import lisp.LispForm;
import lisp.List;
import lisp.SpecialOperator;
import lisp.Symbol;
import lisp.SymbolTable;
import lisp.SyntaxException;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;

public class Defun extends SpecialOperator{
	
	public Defun(){
		super("defun");
	}
	
	public Defun(String name){
		super(name);
	}
	
	protected String getFunctionName() throws SyntaxException{
		return ((Symbol) getParameters().get(0)).getName();
	}
	
	protected void checkArguments(java.util.List<LispForm> parameters) throws SyntaxException{ 
		if(parameters.size() != 3)
			throw new SyntaxException("Special operator defun expects 3 arguments (got " + parameters.size() + " arguments");
		
		if(!(parameters.get(0) instanceof Symbol))
			throw new SyntaxException("First element of defun is expected to be a symbol");
		
		if(!(parameters.get(1) instanceof List))
			throw new SyntaxException("First element of defun is expected to be a list");
	}
	
	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters(); 
		checkArguments(parameters);
		List argumentsList = (List) parameters.get(1);
		
		// remove &rest
		boolean hasRest = argumentsList.hasRest();
		argumentsList.removeRest();
		
		SymbolTable newSymbolTable = new SymbolTable(symbolTable, createAddrMap(argumentsList, "defun"));
		
		Generator.createFunctionClass(getFunctionName());
		
		if(!getFunctionName().equals("Main"))
			if(hasRest)
				Factory.pushMethodVisitor(Factory.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "invokeRest", generateMethodDescriptor(argumentsList.size()), null, null), false);
			else
				Factory.pushMethodVisitor(Factory.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "invoke", generateMethodDescriptor(argumentsList.size()), null, null), false);
		else
			Factory.pushMethodVisitor(Factory.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null), false);
		
		parameters.get(2).compile(newSymbolTable);
		
		if(getFunctionName().equals("Main"))
			Factory.getMethodVisitor().visitInsn(Opcodes.RETURN);
		else
			Factory.getMethodVisitor().visitInsn(Opcodes.ARETURN);
		
		Factory.getMethodVisitor().visitMaxs(0, 0); // stacks sizes are computed automatically thanks to ClassWriter.COMPUTE_FRAMES option. But it is still neccessary to call this method with any arguments (they will be ignored).
		Factory.popMethodVisitor();
		Factory.getClassWriter().visitEnd();
		
		Generator.saveFile(getFunctionName(), Factory.getClassWriter());
		
		if(parent != null && parent.getParent() != null){
			Symbol symbol = new Symbol(getFunctionName());
			symbol.generateYourself(symbolTable);
		}
	}

}
