/* Regression test: the signature of a method in a type nested two levels deep names
 * a member type of the outermost class that is declared AFTER the type holding it
 * ("Cannot resolve type: Extractor" in Spring Boot's JsonWriter). */
public class NestedMemberSignatureForwardSiblingTest {
    class Member {
        interface ValueExtractor<T> {
            Object extract(Object instance);

            default <R> ValueExtractor<R> as(Extractor<T, R> extractor) {
                return (instance) -> extractor.extract(null);
            }
        }

        class Inner {
            Inner copy(Extractor<String, String> e) {
                return this;
            }
        }
    }

    interface Extractor<T, R> {
        R extract(T t);
    }

    public static void main(String[] args) {
        Member.ValueExtractor<String> v = (instance) -> "x";
        Member.ValueExtractor<Integer> w = v.as((s) -> 1);
        if (w == null) {
            throw new RuntimeException("as");
        }
        System.out.println("NestedMemberSignatureForwardSiblingTest passed!");
    }
}
