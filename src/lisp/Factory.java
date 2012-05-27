package lisp;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class Factory {
	private static ClassWriter classWriterInstance;
	private static MethodVisitor methodVisitorInstance;
	
	public static ClassWriter getClassWriter(){
		return classWriterInstance;
	}
	
	public static void setClassWriter(ClassWriter cw){
		classWriterInstance = cw;
	}
	
	public static MethodVisitor getMethodVisitor(){
		return methodVisitorInstance;
	}
	
	public static void setMethodVisitor(MethodVisitor mv){
		methodVisitorInstance = mv;
	}
}
