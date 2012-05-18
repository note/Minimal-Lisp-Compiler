package lisp;

public class SyntaxException extends Exception {
	private String message;
	
	public SyntaxException(String msg){
		message = msg;
	}
}
