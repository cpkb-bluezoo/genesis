import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpRequest.BodyPublishers;
import java.net.http.HttpResponse.BodyHandlers;
import java.time.Duration;
import java.util.prefs.Preferences;

public class NetHttpModuleClassesTest {
	public static void main(String[] args) {
		HttpClient.Builder builder = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(1));
		HttpClient client = builder.build();
		HttpRequest request = HttpRequest.newBuilder(URI.create("http://localhost/"))
			.POST(BodyPublishers.ofByteArray(new byte[] { 1, 2 }))
			.build();
		Object handler = BodyHandlers.discarding();
		if (client == null || !"POST".equals(request.method()) || handler == null || Preferences.MAX_KEY_LENGTH < 1) {
			throw new AssertionError("net.http");
		}
		System.out.println("PASS");
	}
}
