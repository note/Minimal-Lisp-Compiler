package lisp;

import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class Generator {
	
	/**
	 * Stack: -> ref to nth param (if it is list)
	 */
	public static void generateCheckIfList(java.util.List<LispForm> parameters, SymbolTable symbolTable, int nth, String message) throws SyntaxException{
		MethodVisitor mv = Factory.getMethodVisitor();
		
		parameters.get(nth).compile(symbolTable);
		mv.visitInsn(Opcodes.DUP);
		
		Label isList = new Label();
		mv.visitTypeInsn(Opcodes.INSTANCEOF, "lisp/List");
		mv.visitJumpInsn(Opcodes.IFNE, isList);
		
		Generator.generateRuntimeException(message);
		
		mv.visitLabel(isList);
	}
	
	/**
	 * Assumes that reference to casted object is on the top of stack.
	 * Stack: ref to type1 -> ref to type2
	 */
	public static void generateCastToList(){
		MethodVisitor mv = Factory.getMethodVisitor();
		
		mv.visitTypeInsn(Opcodes.CHECKCAST, "lisp/List");
	}
	
	public static void generateRuntimeException(String message){
		MethodVisitor mv = Factory.getMethodVisitor();
		mv.visitLdcInsn(message);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/Runtime", "throwRuntimeException", "(Ljava/lang/String;)V");
	}
	
	public static void generateEmptyList(){
		Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/List", "createEmptyList", "()Llisp/List;");
	}
}
