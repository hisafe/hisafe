package capsis.util;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import jeeb.lib.util.Log;

public class MultiThreaded<T> {

	/**
	 * Start multithreaded process and wait it finish
	 * @param nbThreads
	 * @param params
	 * @param runnable
	 */
	public MultiThreaded(int nbThreads, List<T> params, RunnableWithParam<T,?> runnable) {
		this(nbThreads, params, runnable, null);
	}
	
	public MultiThreaded(int nbThreads, List<T> params, RunnableWithParam<T,?> runnable, Object extra) {
		
		Object stopToken = new Object();
		List<Thread> threads;
		
		BlockingQueue<Object> q = new LinkedBlockingQueue<Object>();
		threads = new LinkedList<Thread>();


		ThreadRunner tr = new ThreadRunner(q, stopToken, extra, runnable);
		// Create threads and queue
		for(int i=0; i<nbThreads; i++) {
			Thread t = new Thread( tr );
			t.start();
			threads.add(t);
		}
		q.addAll(params);
		q.add(stopToken);

		// join
		for(Thread t : threads) {
			try {
				t.join();
			} catch (InterruptedException e) {}
		}
	}

	
	/** Simulation Thread */
	static class ThreadRunner implements Runnable {

		private BlockingQueue<Object> queue;
		private Object stopToken;
		private RunnableWithParam runnable;
		private Object extra;


		public ThreadRunner(BlockingQueue<Object> q, Object st, Object e, RunnableWithParam r) { 
			stopToken = st;
			queue = q; 
			runnable = r;
			extra = e;
		}

		@Override
		public void run() {
			try {
				
				while (true) {
					Object value = queue.take();
					
					if(value == stopToken ) {
						queue.add(stopToken);
						return;
					}

					runnable.run(value, extra);
				}
			}
			catch (Exception e) {
				Log.println(Log.ERROR, "MultiThreaded" , "", e);
				System.err.println(Thread.currentThread().getName() + " " + e);
				e.printStackTrace();
			}
		}
	}
}