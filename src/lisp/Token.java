package lisp;

public class Token {
	public final static int INT = 1;
	public final static int FLOAT = 2;
	public final static int DOUBLE = 3;
	public final static int STRING = 4;
	public final static int OPENING_PARENTHESIS = 5;
	public final static int CLOSING_PARENTHESIS = 6;
	public final static int SYMBOL = 7;
	public final static int COMMENT = 8;
	public final static int EOF = 9;
	
	int code;
	String value;
	
	public Token(int code, String value){
		this.code = code;
		this.value = value;
	}
	
	public int getCode(){
		return code;
	}
	
	public String getValue(){
		return value;
	}
}
