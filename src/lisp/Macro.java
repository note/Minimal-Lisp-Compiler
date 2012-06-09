package lisp;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import lisp.RT.Runtime;

public class Macro extends Symbol{
	String macroName;
	
	public Macro(String name){
		super("macro");
		macroName = name;
	}
	
	public LispForm expandMacros(SymbolTable symbolTable) throws SyntaxException{
		java.util.List<LispForm> parameters = getParameters();
		Class clazz;
		try {
			clazz = Class.forName(macroName);
			Class [] types = new Class[Runtime.getMacroParametersLength(macroName)];
			Object [] args = new Object[parameters.size()];
			for(int i=0; i<parameters.size(); ++i){
				types [i]= LispForm.class;
				args [i]= (Object) parameters.get(i);
			}
			Method m = clazz.getDeclaredMethod("invokeMacro", types);
			
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
}
