/*
 * Regression test: a class implementing an interface method with a
 * COVARIANT return type (no generics/type-variables involved at all) -
 * e.g. "interface Sender { Delivery send(byte[], String); }" implemented
 * as "public DeliveryImpl send(byte[] tag, String name) { ... }" where
 * DeliveryImpl is a subtype of Delivery - used to throw:
 *
 *   java.lang.AbstractMethodError: Receiver class ... does not define or
 *   inherit an implementation of the resolved method ...
 *
 * matching gumdrop's own AMQP 1.0 client, SenderImpl, which implements
 * Amqp1Sender.send(...)/startDelivery(...) (both declared to return the
 * interface type Amqp1OutgoingDelivery) with a covariant, concrete
 * OutgoingDeliveryImpl return type.
 *
 * Root cause: codegen.c's generate_interface_bridges() only ever
 * generated a synthetic bridge method for an interface method whose
 * signature involved a TYPE VARIABLE (a generic erasure gap) - gated by
 * an early "has_type_var" check that skipped everything else entirely.
 * A plain covariant-return override (no generics at all) also needs a
 * bridge for the exact same underlying reason: the JVM only recognizes
 * a class as implementing an interface method when some method on the
 * class has a descriptor EXACTLY matching the interface's own declared
 * one - without a bridge, the concrete "DeliveryImpl send(...)" method
 * alone never satisfies "Delivery send(...)".
 *
 * Fixed by removing the type-variable-only gate and instead comparing
 * the interface method's own (erased) descriptor against the
 * implementation's real descriptor directly - a bridge is needed
 * whenever they differ, which covers both the original generic-erasure
 * case and this plain-covariant-return case uniformly.
 *
 * A second, related gap surfaced once the gate was removed: an
 * interface method's own return type symbol (iface_method->type) is
 * often never resolved at all before this function runs, since nothing
 * previously needed it (an interface's own abstract method has no body
 * to codegen) - the type-variable-only gate had been silently relying
 * on this same unresolved-ness to skip ordinary methods too. Left
 * unresolved, the code treated it as void, producing a bridge whose own
 * declared descriptor said "()V" while its body (correctly built from
 * the resolved implementation method) still emitted e.g. IRETURN - a
 * self-contradictory method (VerifyError: "Method does not expect a
 * return value"). Fixed by force-resolving the interface method's
 * return type from its own AST node (the same semantic_resolve_type()
 * call already used when a method is first registered) whenever it's
 * still unresolved at this point.
 *
 * A FOURTH gap surfaced once the above three were fixed, against gumdrop's
 * real Amqp1Sender.startDelivery(byte[], MessageHeader, MessageProperties,
 * Map, boolean): the bridge's own parameter-loading loop unconditionally
 * emitted ALOAD for every parameter, regardless of its actual type -
 * correct only for reference parameters. A primitive parameter (e.g. the
 * trailing "boolean settled") got ALOAD'd as if it were a reference
 * (VerifyError: "Bad local variable type"), and a wide primitive (long/
 * double) would also have had its local slot advanced by only 1 instead of
 * 2, misaligning every subsequent parameter's slot. Fixed by switching on
 * the interface parameter's own (erased) type kind - mirroring the
 * already-correct equivalent switch in generate_superclass_bridges() above
 * - to pick ILOAD/LLOAD/FLOAD/DLOAD/ALOAD and advance the slot/stack width
 * accordingly.
 *
 * A THIRD gap surfaced once the above two were fixed and this function
 * started firing for far more interface methods than before: its own
 * "find the class's implementation of this interface method" lookup
 * matched candidates by name + parameter COUNT alone - which picks the
 * WRONG overload whenever a class declares more than one same-arity,
 * same-named method, e.g. java.nio.file.Path's own "Path
 * resolve(String)" and "Path resolve(Path)" (both 1 parameter,
 * confirmed against gumdrop's own in-tree
 * org.bluezoo.gumdrop.testsupport.memfs.MemoryPath, which covariantly
 * overrides both) - generating a bridge for one that actually calls the
 * OTHER's real implementation, throwing ClassCastException at runtime
 * for perfectly valid calls. Fixed by matching position-by-position
 * against the interface method's own parameters instead (a new
 * interface_impl_params_match() helper): same count, and at each
 * position either the interface's own parameter is a bare type variable
 * (any concrete implementation type is a valid instantiation) or the
 * two parameter types have identical erased descriptors - a flat whole-
 * list descriptor-string comparison doesn't work here (unlike the
 * closely related, already-fixed superclass-side collision in
 * generate_covariant_override_bridges()) because an interface method's
 * own parameter may itself be an unbounded type variable, whose erased
 * descriptor essentially never equals a real implementation's own
 * concrete parameter type.
 */
public class InterfaceCovariantReturnBridgeVerifyTest {
    interface Delivery {
        String tag();
    }

    static class DeliveryImpl implements Delivery {
        private final String tag;
        DeliveryImpl(String tag) { this.tag = tag; }
        public String tag() { return tag; }
    }

    interface Sender {
        Delivery send(byte[] payload, String name);
    }

    static class SenderImpl implements Sender {
        public DeliveryImpl send(byte[] payload, String name) {
            return new DeliveryImpl(name + ":" + payload.length);
        }
    }

    interface Thing {
        Thing resolve(String s);
        Thing resolve(Thing t);
    }

    static class ThingImpl implements Thing {
        private final String label;
        ThingImpl(String label) { this.label = label; }
        public ThingImpl resolve(String s) { return new ThingImpl(label + "/" + s); }
        public ThingImpl resolve(Thing t) { return new ThingImpl(label + "+" + ((ThingImpl) t).label); }
    }

    /* Matches gumdrop's Amqp1Sender.startDelivery(byte[], MessageHeader,
     * MessageProperties, Map, boolean) / SenderImpl - a covariant-return
     * interface method whose LAST parameter is a primitive, exercising the
     * fourth gap (bridge parameter-loading width/kind). */
    interface Starter {
        Delivery start(byte[] payload, String name, boolean settled);
    }

    static class StarterImpl implements Starter {
        public DeliveryImpl start(byte[] payload, String name, boolean settled) {
            return new DeliveryImpl(name + ":" + payload.length + ":" + settled);
        }
    }

    public static void main(String[] args) {
        Sender s = new SenderImpl();
        Delivery d = s.send(new byte[]{1, 2, 3}, "hello");
        if (!"hello:3".equals(d.tag())) {
            throw new RuntimeException("expected hello:3, got " + d.tag());
        }

        /* Also exercise a plain (non-covariant) interface method in the
         * same class hierarchy, to guard against the "iface_method->type
         * left null, bridge wrongly generated as void" regression this
         * fix's own second part addresses. */
        DeliveryImpl direct = new DeliveryImpl("direct");
        if (!"direct".equals(direct.tag())) {
            throw new RuntimeException("expected direct, got " + direct.tag());
        }

        /* Two same-arity, same-named overloads, both covariantly
         * returning ThingImpl - guards against the third gap
         * (overload-collision in the implementation lookup). */
        Thing base = new ThingImpl("base");
        Thing byString = base.resolve("sub");
        Thing byThing = base.resolve(new ThingImpl("other"));
        if (!"base/sub".equals(((ThingImpl) byString).label)) {
            throw new RuntimeException("expected base/sub, got " + ((ThingImpl) byString).label);
        }
        if (!"base+other".equals(((ThingImpl) byThing).label)) {
            throw new RuntimeException("expected base+other, got " + ((ThingImpl) byThing).label);
        }

        Starter starter = new StarterImpl();
        Delivery started = starter.start(new byte[]{1, 2, 3, 4}, "world", true);
        if (!"world:4:true".equals(started.tag())) {
            throw new RuntimeException("expected world:4:true, got " + started.tag());
        }

        System.out.println("InterfaceCovariantReturnBridgeVerifyTest passed!");
    }
}
