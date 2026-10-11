import java.net.URI;
import java.util.List;

public class OverrideWithArrayTypeArgumentTest {
	static abstract class Sender<U, B> {
		String sent = "";

		protected abstract void post(U endpoint, B body);

		protected abstract B newBody(List<byte[]> list);

		final void send(U endpoint, List<byte[]> list) {
			post(endpoint, newBody(list));
		}
	}

	static class Http extends Sender<URI, byte[]> {
		@Override
		protected void post(URI endpoint, byte[] body) {
			sent = endpoint.getHost() + ":" + body.length;
		}

		@Override
		protected byte[] newBody(List<byte[]> list) {
			int total = 0;
			for (byte[] part : list) {
				total += part.length;
			}
			return new byte[total];
		}
	}

	static class Grid extends Sender<String, int[][]> {
		@Override
		protected void post(String endpoint, int[][] body) {
			sent = endpoint + ":" + body.length + ":" + body[0].length;
		}

		@Override
		protected int[][] newBody(List<byte[]> list) {
			return new int[list.size()][2];
		}
	}

	public static void main(String[] args) {
		Http http = new Http();
		http.send(URI.create("http://example.org/x"), java.util.Arrays.asList(new byte[2], new byte[3]));
		Grid grid = new Grid();
		grid.send("g", java.util.Arrays.asList(new byte[1], new byte[1], new byte[1]));
		if (!"example.org:5".equals(http.sent) || !"g:3:2".equals(grid.sent)) {
			throw new AssertionError(http.sent + " " + grid.sent);
		}
		System.out.println("PASS");
	}
}
