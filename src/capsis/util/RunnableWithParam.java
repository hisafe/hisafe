package capsis.util;

public interface RunnableWithParam<T, U> {
	
	
	public void run(T param, U extra) throws Exception;

}
