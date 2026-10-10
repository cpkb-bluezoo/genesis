package p;

import p.F.Fixture;

/* Regression test: "Fixture.create()" where Fixture is imported as p.F.Fixture
 * while another class of the package (p.G, analysed first) also nests a
 * Fixture. Loading G entered its nested Fixture under the simple name, and
 * the static call's receiver resolved to that one - which has no create() -
 * instead of the single-type import ("cannot convert <unknown> to
 * p.F$Fixture"). Mirrors gumdrop's QuicConnectionDirectTest. */
public class ImportedNestedReceiverTest {
    public static void main(String[] args) {
        G g = new G();
        Fixture f = Fixture.create();
        if (f.get() + g.x() != 8) {
            throw new RuntimeException("wrong");
        }
        System.out.println("ImportedNestedReceiverTest passed!");
    }
}
