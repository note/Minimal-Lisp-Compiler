package lisp;

import java.util.HashMap;

import lisp.specialoperators.Defun;

public class SymbolTable {
	private static final HashMap<String, Class<? extends SpecialOperator>> specialOperatorsMap = new HashMap<String, Class<? extends SpecialOperator>>();
	
	static{
		specialOperatorsMap.put("defun", Defun.class);
	}
	
	public boolean isSpecialOperator(String name){
		return specialOperatorsMap.containsKey(name);
	}
	
	public SpecialOperator getNewSpecialOperatorInstance(String name){
		Class<? extends SpecialOperator> clazz = specialOperatorsMap.get(name);
		if(clazz != null)
			try {
				return clazz.newInstance();
			} catch (InstantiationException e) {
				return null;
			} catch (IllegalAccessException e) {
				return null;
			}
		return null;
	}
}
