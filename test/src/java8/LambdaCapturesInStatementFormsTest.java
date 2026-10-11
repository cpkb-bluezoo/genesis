public class LambdaCapturesInStatementFormsTest {
	static void keep(Runnable r) {
		r.run();
	}

	public static void main(String[] args) throws Exception {
		String s = "s";
		int n = 1;
		Object o = "o";
		StringBuilder out = new StringBuilder();
		keep(() -> {
			try {
				out.append(1);
			}
			finally {
				out.append(s);
			}
		});
		keep(() -> {
			lbl: for (;;) {
				out.append(s);
				break lbl;
			}
		});
		keep(() -> {
			switch (n) {
				case 1:
					out.append(s);
					break;
				default:
			}
		});
		keep(() -> {
			synchronized (o) {
				out.append(s);
			}
		});
		keep(() -> {
			for (String x : new String[] { s, s }) {
				out.append(x);
			}
		});
		keep(() -> {
			try (java.io.StringReader reader = new java.io.StringReader(s)) {
				out.append((char) reader.read());
			}
			catch (java.io.IOException ex) {
				throw new IllegalStateException(ex);
			}
		});
		if (!"1sssssss".equals(out.toString())) {
			throw new AssertionError(out.toString());
		}
		System.out.println("PASS");
	}
}
