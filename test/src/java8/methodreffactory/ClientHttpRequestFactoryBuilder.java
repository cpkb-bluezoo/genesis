package methodreffactory;

public interface ClientHttpRequestFactoryBuilder {
    static HttpComponentsClientHttpRequestFactoryBuilder httpComponents() {
        return new HttpComponentsClientHttpRequestFactoryBuilder() {
        };
    }
}
