package lisp;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class Generator {
	
	private static void generateCheckIfInstanceOf(LispForm form, SymbolTable symbolTable, String type, String message) throws SyntaxException{
		MethodVisitor mv = Factory.getMethodVisitor();
		
		form.compile(symbolTable);
		mv.visitInsn(Opcodes.DUP);
		
		Label isList = new Label();
		mv.visitTypeInsn(Opcodes.INSTANCEOF, type);
		mv.visitJumpInsn(Opcodes.IFNE, isList);
		
		Generator.generateRuntimeException(message);
		
		mv.visitLabel(isList);
	}
	
	public static void generateCheckIfList(LispForm form, SymbolTable symbolTable, String message) throws SyntaxException{
		generateCheckIfInstanceOf(form, symbolTable, "lisp/List", message);
	}
	
	public static void generateCheckIfSymbol(LispForm form, SymbolTable symbolTable, String message) throws SyntaxException{
		generateCheckIfInstanceOf(form, symbolTable, "lisp/Symbol", message);
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
}
