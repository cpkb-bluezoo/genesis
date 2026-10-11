package p;

import java.util.ArrayList;
import java.util.List;

public class SrvFactory {
	private final List<SrvCustomizer> customizers = new ArrayList<SrvCustomizer>();

	public void addCustomizers(SrvCustomizer... added) {
		for (SrvCustomizer c : added) {
			customizers.add(c);
		}
	}

	public Srv build() {
		Srv srv = new Srv();
		for (SrvCustomizer c : customizers) {
			srv = c.apply(srv);
		}
		return srv;
	}
}
