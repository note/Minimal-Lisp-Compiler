package lisp.RT;


import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Currency;
import java.util.HashMap;

import lisp.LispRuntimeException;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

class ClassFilter implements FilenameFilter {
	public boolean accept(File dir, String name) {
		return (name.endsWith(".class"));
	}
}

class ClassPrinter extends ClassVisitor {
	public ClassPrinter() {
		super(Opcodes.ASM4);
	}

	public void visit(int version, int access, String name, String signature,
			String superName, String[] interfaces) {
	}

	public void visitSource(String source, String debug) {
	}

	public void visitOuterClass(String owner, String name, String desc) {
	}

	public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
		return null;
	}

	public void visitAttribute(Attribute attr) {
	}

	public void visitInnerClass(String name, String outerName,
			String innerName, int access) {
	}

	public FieldVisitor visitField(int access, String name, String desc,
			String signature, Object value) {
		return null;
	}

	public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
		if(name.equals("invoke"))
			// if desc = "()IValue;" then desc.split(";").length == 1
			Runtime.addFunction(desc.split(";").length - 1);
		return null;
	}

	public void visitEnd() {
		
	}
}

public class Runtime {
	private static HashMap<String, Integer> functions = new HashMap<String, Integer>();
	private static HashMap<String, String> aliases = new HashMap<String, String>();
	private static String currentFunctionName;
	
	static{
		aliases.put("+", "_plus");
		init();
	}
	
	private static String[] getClassNames(String dirname) {
		File dir = new File(dirname);
		FilenameFilter filter = new ClassFilter();
		String[] files = dir.list(filter);
		for (int i = 0; i < files.length; ++i)
			files[i] = files[i].split("\\.")[0];
		return files;
	}

	public static void init(){
		String[] names = getClassNames(".");
		ClassPrinter cp = new ClassPrinter();
		for(int i=0; i<names.length; ++i){
			currentFunctionName = names[i];
			ClassReader cr;
			try {
				cr = new ClassReader(new FileInputStream(names[i] + ".class"));
				cr.accept(cp, 0);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		}
	}
	
	public static void addFunction(int num){
		functions.put(currentFunctionName, num);
	}
	
	/**
	 * 
	 * @param name - name of function or alias
	 * @return - name of function (if parameter name was alias it will return function name)
	 * @example for "+" will return "_plus"
	 */
	public static String getFunctionName(String name){
		if(aliases.containsKey(name))
			return aliases.get(name);
		return name;
	}
	
	public static int getFunctionParametersLength(String name){
		Integer res = functions.get(getFunctionName(name));
		if(res != null)
			return res;
		return -1;
	}
	
	public static void throwRuntimeException(String message){
		throw new LispRuntimeException(message);
	}
}
