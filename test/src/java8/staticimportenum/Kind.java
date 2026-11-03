package staticimportenum;

public enum Kind {
    ALPHA(1L), BETA(2L);

    private final Long id;

    Kind(long id) {
        this.id = id;
    }

    public Long getId() {
        return id;
    }
}
