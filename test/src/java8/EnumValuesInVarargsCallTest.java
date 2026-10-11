import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public enum EnumValuesInVarargsCallTest {
	ONE, TWO, THREE;

	static int count() {
		return EnumValuesInVarargsCallTest.values().length;
	}

	static List<EnumValuesInVarargsCallTest> latestFirst() {
		List<EnumValuesInVarargsCallTest> all = Arrays.asList(EnumValuesInVarargsCallTest.values());
		Collections.reverse(all);
		return all;
	}

	public static void main(String[] args) {
		if (count() != 3 || latestFirst().get(0) != THREE
				|| Arrays.asList(EnumValuesInVarargsCallTest.values()).size() != 3
				|| EnumValuesInVarargsCallTest.valueOf("TWO").ordinal() != 1) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
