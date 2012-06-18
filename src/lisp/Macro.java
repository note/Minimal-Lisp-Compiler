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
			int parametersLength = Runtime.getMacroParametersLength(macroName);
			Class [] types = new Class[parametersLength];
			Object [] args = new Object[parametersLength];
			Method method = null;
			
			if(Runtime.hasRest(macroName)){
				for(int i=0; i<parametersLength-1; ++i){
					types [i]= LispForm.class;
					args [i]= (Object) parameters.get(i);
				}
				types[parametersLength-1] = LispForm.class;
				List lastArg = new List();
				for(int i=parametersLength-1; i<parameters.size(); ++i)
					lastArg.addChild(parameters.get(i));
				args[parametersLength-1] = lastArg;
				
				method = clazz.getDeclaredMethod("invokeMacroRest", types);
			}else{
				for(int i=0; i<parametersLength; ++i){
					types [i]= LispForm.class;
					args [i]= (Object) parameters.get(i);
				}
				method = clazz.getDeclaredMethod("invokeMacro", types);
			}
			
			return (LispForm) method.invoke(null, args);
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
