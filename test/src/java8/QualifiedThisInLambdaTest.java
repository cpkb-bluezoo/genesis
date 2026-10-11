import java.util.concurrent.Callable;

public class QualifiedThisInLambdaTest {
	private final StringBuilder log = new StringBuilder();

	void stop() {
		this.log.append("stop;");
	}

	void start(String how) {
		this.log.append("start-").append(how).append(';');
	}

	void restart(String how) throws Exception {
		Callable<Object> call = () -> {
			QualifiedThisInLambdaTest.this.stop();
			QualifiedThisInLambdaTest.this.start(how);
			return new Object();
		};
		call.call();
	}

	class Inner {
		void run(String how) throws Exception {
			Callable<Object> call = () -> {
				QualifiedThisInLambdaTest.this.start(how);
				Inner.this.toString();
				return null;
			};
			call.call();
		}
	}

	public static void main(String[] args) throws Exception {
		QualifiedThisInLambdaTest test = new QualifiedThisInLambdaTest();
		test.restart("now");
		test.new Inner().run("inner");
		if (!"stop;start-now;start-inner;".equals(test.log.toString())) {
			throw new AssertionError(test.log.toString());
		}
		System.out.println("PASS");
	}
}
