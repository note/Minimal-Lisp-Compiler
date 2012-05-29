import lisp.LispForm;
import lisp.List;

public class print {
	public static LispForm invoke(LispForm arg){
		System.out.println(arg.toString());
		return List.createEmptyList();
	}
}
