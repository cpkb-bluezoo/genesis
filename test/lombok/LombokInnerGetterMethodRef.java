import lombok.Getter;
import java.util.List;

class LombokInnerGetterMethodRef {
    @Getter
    static class Inner {
        private List<String> labels;
    }

    void use(List<Inner> inners) {
        inners.stream().map(Inner::getLabels);
    }
}
