# trace-spine

Wire-discipline library and CI harness that enforces W3C traceparent propagation
across every wire crossing in the architecture.

## What this is

trace-spine is the **contract enforcement** layer for distributed tracing.
It is not a tracing backend, not a collector, not a sampling policy.

The invariant: every request, message, span, and event carries a W3C `traceparent`
that resolves to the same trace id throughout its causal cone.

## Quick start

```yaml
# .trace-spine.yaml in your service repo
version: 1
service_name: checkout
ingress_designated: false
crossings:
  http_inbound:
    - path: /api/checkout
      framework: rails
```

## Spec

| Document                | Purpose                              |
|-------------------------|--------------------------------------|
| spec/L0-claims.org      | Named invariants (I-spine-*)        |
| spec/L1-wire.org        | Wire format (W3C Trace Context v00) |
| spec/L1-contracts.org   | Function contracts for adapters     |
| spec/L2-properties.org  | Property tests                      |
| spec/cprr.org           | Conjecture tracking                 |

## Wire format

```
traceparent: 00-{trace-id}-{parent-id}-{flags}
             00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01
```

Validation regex: `^00-[0-9a-f]{32}-[0-9a-f]{16}-[0-9a-f]{2}$`

## Adapters

| Language/Substrate | Status  |
|--------------------|---------|
| Clojure (JVM)      | Planned |
| ClojureScript      | Planned |
| Ruby (Rails)       | Planned |
| Babashka           | Planned |
| aq (NDJSON)        | Planned |
| Go                 | Planned |

## Conformance

Services declare their wire crossings in `.trace-spine.yaml` and run the
conformance harness in CI:

```bash
cd conformance && ./run.sh
```

## Development

The Clojure code follows the aygp-dr Clojure standard. Every sub-project
(`lib/clj` and `platform/*`) has a `deps.edn` with `:dev`, `:test`, `:lint`
and `:fmt` aliases, plus its own `bb.edn`. The root `bb.edn` runs the same
task in each sub-project; inside a sub-project, the task runs only that
project.

| Task                    | What it does                                                        |
|-------------------------|---------------------------------------------------------------------|
| `bb test`               | each sub-project's tests (`clojure -M:test`: kaocha; test-runner in wallet) |
| `bb lint`               | clj-kondo on src/test/dev, failing on errors only                   |
| `bb fmt` / `bb fmt:fix` | cljfmt check / fix                                                  |
| `bb check`              | lint + fmt + test in every sub-project (what CI runs)               |
| `bb projects`           | list the sub-projects                                               |

### How the existing harness maps onto `bb check`

| Entry point                                    | Now                                                              |
|------------------------------------------------|------------------------------------------------------------------|
| `make test` / `make test-all`                  | `bb test`, which now includes `lib/clj`                          |
| `make clj-lint`, `make -C platform/<svc> lint` | `bb lint`: no global clj-kondo needed, and warnings no longer fail |
| `make lint`                                    | `bb lint` + `bin/org-lint`                                       |
| `make ci`                                      | `bb check` + `make org-lint` + `make c4-check`                   |
| CI `lint` job and `test` matrix                | the single `check` job in `ci.yml` (`bb check`)                  |
| `lint.yml` clj-kondo job                       | removed, since `check` covers it; the org-lint, deps.edn and Emacs org-lint jobs are unchanged |
| `cd conformance && ./run.sh`                   | the harness script is not in the repo yet; the L2 properties it would run for the Clojure adapter are in `lib/clj/test/trace_spine/properties_test.clj` |

### Specs

`lib/clj/src/trace_spine/specs.clj` is the monorepo's one clojure.spec
definition of the wire format. `::traceparent` is version `00`, lowercase hex,
32/16/2 digits, with neither id all zeros (`spec/L1-wire.org`). It also has
`::tracestate`, `::trace-context` and `::carrier`, each with a generator, plus
`gen-malformed-traceparent` for near misses. Every platform service requires
it (`[trace-spine.specs :as ts]`) rather than re-deriving the rules, and keeps
its own `src/<svc>/specs.clj` for its domain data. Public fns carry an
`s/fdef` right after their `defn`. Each `specs_test.clj` runs `stest/check`
over the pure fns, `lib/clj`'s `properties_test.clj` runs the L2 properties
(roundtrip, continuation, no_fabrication) with test.check, and `clj -M:dev`
instruments everything at the REPL. Generators are built lazily, so the
specs load without test.check on the classpath.

## Documentation

- [ADR-0001: Record decisions](docs/architecture/adr/0001-record-decisions.md)
- [ADR-0002: W3C Trace Context](docs/architecture/adr/0002-w3c-trace-context-version-00.md)
- [ADR-0003: No internal origination](docs/architecture/adr/0003-no-internal-origination.md)
- [ADR-0004: 50µs budget](docs/architecture/adr/0004-50us-propagation-budget.md)
- [ADR-0005: Cross-substrate test](docs/architecture/adr/0005-cross-substrate-chain-property.md)

## Team

| Role           | Scope                                           |
|----------------|------------------------------------------------|
| spec-keeper    | Edit spec, gate L0→L3, sign exemptions         |
| spine-engineer | Libraries, harness, middleware                 |
| adversary      | Chaos testing (agent-fillable under review)    |

## License

TBD
