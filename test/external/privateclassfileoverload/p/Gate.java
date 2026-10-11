package p;

public class Gate {
	public interface Request {}
	public interface HttpRequest extends Request {}
	public static class MockRequest implements HttpRequest {}

	public String one(Request request) {
		return "public";
	}

	private String one(HttpRequest request) {
		return "private";
	}

	public String two(Request request) {
		return "public";
	}

	private String two(HttpRequest request) {
		return "private";
	}

	public String three(Request request) {
		return "public";
	}

	private String three(HttpRequest request) {
		return "private";
	}

	public String four(Request request) {
		return "public";
	}

	private String four(HttpRequest request) {
		return "private";
	}

	public String five(Request request) {
		return "public";
	}

	private String five(HttpRequest request) {
		return "private";
	}

	public String six(Request request) {
		return "public";
	}

	private String six(HttpRequest request) {
		return "private";
	}

	public String seven(Request request) {
		return "public";
	}

	private String seven(HttpRequest request) {
		return "private";
	}

	public String eight(Request request) {
		return "public";
	}

	private String eight(HttpRequest request) {
		return "private";
	}

	public String inside(HttpRequest request) {
		return one(request) + two(request);
	}
}
