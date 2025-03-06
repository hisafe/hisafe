package capsis.util.methodprovider;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import capsis.kernel.MethodProvider;

/**
 * CompositeProvider allows to compose multiple MethodProvider in a single class
 * It is based on the Proxy concept of Java which allow to redirect dynamically method call
 * @author sdufour
 *
 */
public class CompositeProvider  implements InvocationHandler {
	
	List<Object> objects;
	
	/** Factory 
	 * @throws Throwable 
	 * */
	static public MethodProvider create(Class<?>[] list) throws Throwable {
		
		CompositeProvider handler = new CompositeProvider(list);
		Set<Class<?>> interfaces = new HashSet<Class<?>>();
		
		// get all interfaces
		for(Class<?> c : list) {
			for(Class<?> c2 : c.getInterfaces()) {
				interfaces.add(c2);
			}
		}
		
		for(Class<?> c2 : MethodProvider.class.getInterfaces()) {
			interfaces.add(c2);
			System.out.println(c2.toString());
		}
		

		Class<?>[] i = (Class<?>[]) interfaces.toArray(new Class<?>[0]);
		return (MethodProvider) Proxy.newProxyInstance(handler.getClass().getClassLoader(), 
				i, handler);
		
	}

	
	/** protected Constructor : use factory instead 
	 * @throws Throwable 
	 */
	protected CompositeProvider(Class<?>[] clss) throws Throwable {
		
		objects = new ArrayList<Object>();
	
		// Build an instance of each class
		for(Class<?> c : clss ) {
				Constructor cons = c.getConstructor();
				Object o = cons.newInstance();
				objects.add(o);
			
		}
	}
	
	
	/** Redirection of method call */
	@Override
	public Object invoke(Object obj, Method m, Object[] args)
			throws Throwable {
	
		// Try each object
		for(Object o : objects) {
			try {
				return m.invoke(o, args);
			}
			catch (NoSuchMethodError e) {
				continue;
			}
			catch (IllegalArgumentException e) {
				continue;
			}
		}
		
		throw new NoSuchMethodError("CompositeProvider : Cannot execute method");
	}
	
}

