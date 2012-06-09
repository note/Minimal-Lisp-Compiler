package lisp.specialoperators;

import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.Generator;
import lisp.LispForm;
import lisp.List;
import lisp.SpecialOperator;
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
		
		SymbolTable newSymbolTable = new SymbolTable(symbolTable, createAddrMap((List) parameters.get(1), "defmacro"));
		
		Generator.createFunctionClass(getFunctionName());
		
		Factory.pushMethodVisitor(Factory.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "invokeMacro", generateMethodDescriptor(((List) parameters.get(1)).size()), null, null), false);
		
		parameters.get(2).compile(newSymbolTable);
		
		Factory.getMethodVisitor().visitInsn(Opcodes.ARETURN);
		
		Factory.getMethodVisitor().visitMaxs(0, 0); // stacks sizes are computed automatically thanks to ClassWriter.COMPUTE_FRAMES option. But it is still neccessary to call this method with any arguments (they will be ignored).
		Factory.popMethodVisitor();
		Factory.getClassWriter().visitEnd();
		
		Generator.saveFile(getFunctionName(), Factory.getClassWriter());
	}

}
