import lombok.*;

@lombok.Getter
@Slf4j
class LombokQualifiedAndStarImport {
    void f() {
        log.info("x");
    }
}
