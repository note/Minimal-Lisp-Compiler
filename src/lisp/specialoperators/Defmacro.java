package lisp.specialoperators;

import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.Generator;
import lisp.LispForm;
import lisp.List;
import lisp.SpecialOperator;
import lisp.Symbol;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class Defmacro extends Defun {
	
	public Defmacro(){
		super("defmacro");
	}

	public Defmacro(String name) {
		super("defmacro");
	}
	
	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {		
		java.util.List<LispForm> parameters = getParameters(); 
		checkArguments(parameters);
		
		List argumentsList = (List) parameters.get(1);
		
		// remove &rest
		boolean hasRest = argumentsList.hasRest();
		argumentsList.removeRest();
		
		SymbolTable newSymbolTable = new SymbolTable(symbolTable, createAddrMap(argumentsList, "defmacro"));
		
		Generator.createFunctionClass(getFunctionName());
		
		if(hasRest)
			Factory.pushMethodVisitor(Factory.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "invokeMacroRest", generateMethodDescriptor(argumentsList.size()), null, null), false);
		else
			Factory.pushMethodVisitor(Factory.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "invokeMacro", generateMethodDescriptor(argumentsList.size()), null, null), false);
		
		parameters.get(2).compile(newSymbolTable);
		
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
