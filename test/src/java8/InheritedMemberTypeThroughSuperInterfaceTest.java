import java.util.*;

/* Regression test: a member type inherited through an interface that itself
 * extends the interface declaring it (JLS 8.5) was not found by its simple
 * name: "Cannot resolve type: ConvertiblePair" in Spring Boot, where a class
 * implements ConditionalGenericConverter, which extends GenericConverter, which
 * declares ConvertiblePair. (Found when the interface is named through an
 * on-demand import.) */
interface Sub extends Map<String, String> {
}

interface SubSub extends Sub {
}

abstract class ViaOne implements Sub {
    Set<Entry<String, String>> entries() {
        return entries();
    }
}

abstract class ViaTwo implements SubSub {
    Entry<String, String> entry = new AbstractMap.SimpleEntry<>("k", "v");
}

abstract class ViaSuperclass extends ViaTwo {
    Entry<String, String> other = entry;
}

public class InheritedMemberTypeThroughSuperInterfaceTest {
    public static void main(String[] args) {
        Map.Entry<String, String> e = new AbstractMap.SimpleEntry<>("a", "b");
        if (!"a".equals(e.getKey())) {
            throw new RuntimeException(e.getKey());
        }
        System.out.println("InheritedMemberTypeThroughSuperInterfaceTest passed!");
    }
}
