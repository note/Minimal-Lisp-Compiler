package lisp.specialoperators;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import lisp.Factory;
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

	private void createFunctionClass(String className){
		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
		Factory.setClassWriter(cw);
		
		cw.visit(Opcodes.V1_5, Opcodes.ACC_PUBLIC, className, null, "java/lang/Object", new String[] {});
	}
	
	private void saveFile(String functionName, ClassWriter cw){
		try {
			FileOutputStream out = new FileOutputStream(functionName + ".class");
			out.write(cw.toByteArray());
			out.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private String getFunctionName() throws SyntaxException{
		return ((Symbol) getParameters().get(0)).getName();
	}
	
	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {		
		java.util.List<LispForm> parameters = getParameters(); 
		if(parameters.size() != 3)
			throw new SyntaxException("Special operator defun expects 3 arguments (got " + parameters.size() + " arguments");
		
		if(!(parameters.get(0) instanceof Symbol))
			throw new SyntaxException("First element of defun is expected to be a symbol");
		
		if(!(parameters.get(1) instanceof List))
			throw new SyntaxException("First element of defun is expected to be a list");
		
		createFunctionClass(getFunctionName());
		
		if(!getFunctionName().equals("Main"))
			Factory.setMethodVisitor(Factory.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "invoke", getMethodDescriptor(((List) parameters.get(1)).size()), null, null));
		else
			Factory.setMethodVisitor(Factory.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null));
		
		parameters.get(2).compile(symbolTable);
		
		if(!getFunctionName().equals("Main"))
			Factory.getMethodVisitor().visitInsn(Opcodes.IRETURN);
		else
			Factory.getMethodVisitor().visitInsn(Opcodes.RETURN);
		
		Factory.getMethodVisitor().visitMaxs(10, 10); //todo: solve this problem
		Factory.getClassWriter().visitEnd();
	
		saveFile(getFunctionName(), Factory.getClassWriter());
	}

}
