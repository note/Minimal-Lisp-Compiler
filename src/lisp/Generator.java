package lisp;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class Generator {
	
	/**
	 * Stack: variable_to_check -> variable_to_check
	 * 
	 * @param form
	 * @param symbolTable
	 * @param type - fully qualified type name
	 * @param message
	 * @throws SyntaxException
	 */
	private static void generateCheckIfInstanceOf(SymbolTable symbolTable, String type, String message) throws SyntaxException{
		MethodVisitor mv = Factory.getMethodVisitor();
		
		mv.visitInsn(Opcodes.DUP);
		
		Label isList = new Label();
		mv.visitTypeInsn(Opcodes.INSTANCEOF, type);
		mv.visitJumpInsn(Opcodes.IFNE, isList);
		
		Generator.generateRuntimeException(message);
		
		mv.visitLabel(isList);
	}
	
	public static void generateCheckIfList(SymbolTable symbolTable, String message) throws SyntaxException{
		generateCheckIfInstanceOf(symbolTable, "lisp/List", message);
	}
	
	public static void generateCheckIfSymbol(SymbolTable symbolTable, String message) throws SyntaxException{
		generateCheckIfInstanceOf(symbolTable, "lisp/Symbol", message);
	}
	
	/**
	 * Assumes that reference to casted object is on the top of stack.
	 * Stack: ref to type1 -> ref to type2
	 */
	public static void generateCastToList(){
		MethodVisitor mv = Factory.getMethodVisitor();
		
		mv.visitTypeInsn(Opcodes.CHECKCAST, "lisp/List");
	}
	
	public static void generateCastToSymbol(){
		MethodVisitor mv = Factory.getMethodVisitor();
		
		mv.visitTypeInsn(Opcodes.CHECKCAST, "lisp/Symbol");
	}
	
	public static void generateRuntimeException(String message){
		MethodVisitor mv = Factory.getMethodVisitor();
		mv.visitLdcInsn(message);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/Runtime", "throwRuntimeException", "(Ljava/lang/String;)V");
	}
	
	public static void generateEmptyList(){
		Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/List", "createEmptyList", "()Llisp/List;");
	}
	
	public static void createFunctionClass(String className){
		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
		Factory.pushClassWriter(cw);
		
		cw.visit(Opcodes.V1_5, Opcodes.ACC_PUBLIC, className, null, "java/lang/Object", new String[] {});
	}
	
	public static void saveFile(String functionName, ClassWriter cw){
		try {
			FileOutputStream out = new FileOutputStream(functionName + ".class");
			out.write(cw.toByteArray());
			out.close();
			Factory.popClassWriter();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public static void generatePushParameters(SymbolTable st, java.util.List<LispForm> parameters) throws SyntaxException {
		MethodVisitor mv = Factory.getMethodVisitor();
		mv.visitLdcInsn(parameters.size());
		mv.visitTypeInsn(Opcodes.ANEWARRAY, "lisp/LispForm");
		
		for (int i = 0; i < parameters.size(); ++i)
			if (parameters.get(i) instanceof Int
					|| parameters.get(i) instanceof List
					|| parameters.get(i) instanceof Variable){
				
				mv.visitInsn(Opcodes.DUP); //arrayref
				mv.visitLdcInsn(i); //index
				parameters.get(i).compile(st); //value
				
				mv.visitInsn(Opcodes.AASTORE);
			}else
				throw new SyntaxException("Cannot generate code for pushing function parameter");
	}
}
