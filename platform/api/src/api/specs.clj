(ns api.specs
  "Data specs for the API Gateway (https://clojure.org/guides/spec).

  Headers, trace context and the traceparent header come from
  trace-spine.specs (lib/clj), the monorepo's single definition of the W3C
  wire format. Function specs (s/fdef) live next to each defn in api.core."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [api.cart-item :as-alias item]
            [api.graphql :as-alias gql]
            [api.identity :as-alias id]
            [api.request :as-alias req]
            [trace-spine.specs :as ts]))

;; =============================================================================
;; wrap-trace-context options
;; =============================================================================

(s/def ::is-ingress? boolean?)
(s/def ::strict-mode? boolean?)
(s/def ::trace-opts (s/keys :opt-un [::is-ingress? ::strict-mode?]))

;; =============================================================================
;; Requests, as the handlers see them after the middleware stack
;; =============================================================================

(defn- gen-id [prefix]
  (gen/fmap #(str prefix %) (gen/large-integer* {:min 1 :max 99999})))

(s/def ::id/sub (s/with-gen (s/and string? seq) #(gen-id "user-")))
(s/def ::id/roles (s/coll-of string? :kind vector? :gen-max 2))
(s/def ::id/premium? boolean?)
(s/def ::id/internal? boolean?)
(s/def ::req/identity
  (s/nilable (s/keys :req-un [::id/sub] :opt-un [::id/roles ::id/premium? ::id/internal?])))

(s/def ::req/trace-context (s/nilable ::ts/trace-context))
(s/def ::req/query-params (s/map-of keyword? string? :gen-max 3))
(s/def ::req/id (s/with-gen (s/and string? seq) #(gen-id "prod_")))
(s/def ::req/path-params (s/keys :opt-un [::req/id]))

(s/def ::item/product_id (s/with-gen (s/and string? seq) #(gen-id "prod_")))
(s/def ::item/variant_id (s/nilable string?))
(s/def ::item/quantity pos-int?)
(s/def ::cart-item (s/keys :opt-un [::item/product_id ::item/variant_id ::item/quantity]))

(s/def ::gql/query (s/with-gen string? #(gen/elements ["{ products { id name } }" "{ cart { id } }"])))
(s/def ::gql/variables (s/nilable (s/map-of keyword? string? :gen-max 2)))
(s/def ::gql/operation_name (s/nilable string?))
(s/def ::graphql-body (s/keys :opt-un [::gql/query ::gql/variables ::gql/operation_name]))

(s/def ::req/body
  (s/nilable (s/nonconforming (s/or :cart-item ::cart-item :graphql ::graphql-body))))

(s/def ::request
  (s/keys :opt-un [:trace-spine.ring/headers ::req/trace-context ::req/identity
                   ::req/query-params ::req/path-params ::req/body]))

(s/def ::response ::ts/ring-response)

;; =============================================================================
;; Server
;; =============================================================================

(s/def ::port (s/int-in 1 65536))
(s/def ::server-opts (s/keys :opt-un [::port]))
