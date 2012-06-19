package lisp.RT;


import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Currency;
import java.util.HashMap;

import lisp.LispForm;
import lisp.LispRuntimeException;
import lisp.List;
import lisp.SymbolTable;

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
			// if desc = "()LispForm;" then desc.split(";").length == 1
			Runtime.addFunction(desc.split(";").length - 1, false);
		if(name.equals("invokeRest"))
			Runtime.addFunction(desc.split(";").length - 1, true);
		if(name.equals("invokeMacro"))
			Runtime.addMacro(desc.split(";").length - 1, false);
		if(name.equals("invokeMacroRest"))
			Runtime.addMacro(desc.split(";").length - 1, true);
		return null;
	}

	public void visitEnd() {
		
	}
}

public class Runtime {
	private static HashMap<String, Integer> functions = new HashMap<String, Integer>();
	private static HashMap<String, Integer> macros = new HashMap<String, Integer>();
	private static HashMap<String, Boolean> rest = new HashMap<String, Boolean>();
	private static String currentFunctionName;
	private static SymbolTable st = new SymbolTable();
	
	static{
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

	private static void initForDir(String dir){
		String[] names = getClassNames(dir);
		ClassPrinter cp = new ClassPrinter();
		for(int i=0; i<names.length; ++i){
			currentFunctionName = names[i];
			ClassReader cr;
			try {
				cr = new ClassReader(new FileInputStream(dir + "/" + names[i] + ".class"));
				cr.accept(cp, 0);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		}
	}
	
	public static void init(){
		initForDir("generated");
		initForDir("std_lib");
	}
	
	public static void addFunction(int num, boolean hasRest){
		functions.put(currentFunctionName, num);
		rest.put(currentFunctionName, hasRest);
	}
	
	public static void addMacro(int num, boolean hasRest){
		macros.put(currentFunctionName, num);
		rest.put(currentFunctionName, hasRest);
	}
	
	public static int getFunctionParametersLength(String name){
//		System.out.println("!!!!!!!!!");
//		System.out.println(name);
		name = st.getFunctionName(name);
//		System.out.println(name);
		Integer res = functions.get(name);
		if(res != null)
			return res;
		return -1;
	}
	
	public static int getMacroParametersLength(String name){
		Integer res = macros.get(name);
		name = st.getFunctionName(name);
		if(res != null)
			return res;
		return -1;
	}
	
	public static int getParametersLength(String name){
		Integer res = functions.get(name);
		if(res != null)
			return res;
		res = macros.get(name);
		if(res != null)
			return res;
		return -1;
	}
	
	public static boolean isMacro(String name){
		return macros.containsKey(st.getFunctionName(name));
	}
	
	public static void throwRuntimeException(String message){
		throw new LispRuntimeException(message);
	}
	
	public static boolean isNil(LispForm form){
		return (form instanceof List) && ((List) form).isEmpty();
	}
	
	public static boolean hasRest(String name){
		return rest.get(st.getFunctionName(name));
	}
	
	public static LispForm funcall(String functionName, LispForm [] args){
		Class clazz;
		functionName = st.getFunctionName(functionName);
		try {
			clazz = Class.forName(functionName);
			Class [] types = new Class[args.length];
			for(int i=0; i<args.length; ++i)
				types [i]= LispForm.class;
			Method m = clazz.getDeclaredMethod("invoke", types);
			return (LispForm) m.invoke(null, args);
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	
	public static LispForm callWithRest(String functionName, LispForm [] args){
		Class clazz;
		functionName = st.getFunctionName(functionName);
		try {			
			int parametersLength = Runtime.getParametersLength(functionName);
			
			LispForm [] actualArgs = new LispForm[parametersLength];
			clazz = Class.forName(functionName);
			Class [] types = new Class[parametersLength];
			
			for(int i=0; i<parametersLength-1; ++i){
				types [i]= LispForm.class;
				actualArgs[i] = args[i];
			}
			types[parametersLength-1] = LispForm.class;
				
			List lastArg = new List();
			for(int i = parametersLength-1; i<args.length; ++i)
				lastArg.addChild(args[i]);
			actualArgs[parametersLength-1] = lastArg;
			
			Method m = clazz.getDeclaredMethod("invokeRest", types);		
			
			return (LispForm) m.invoke(null, actualArgs);
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	
	public static LispForm apply(String functionName, int expectedNumberOfParameters, List arguments){
		LispForm [] tmp = new LispForm[1];
		functionName = st.getFunctionName(functionName);
		
		if(Runtime.hasRest(functionName)){
			if(arguments.size() < expectedNumberOfParameters)
				Runtime.throwRuntimeException("Invalid number of arguments: " + arguments.size());
			return Runtime.callWithRest(functionName, arguments.getChildren().toArray(tmp));
		}else{
			if(arguments.size() != expectedNumberOfParameters)
				Runtime.throwRuntimeException("Invalid number of arguments: " + arguments.size());
			return Runtime.funcall(functionName, arguments.getChildren().toArray(tmp));
		}
	}
}
