# Addendum: Structuring an Application in Haskell

A companion to *Structuring an Application*, recording what changed when
the model was applied to a Haskell service using the `effectful` effect
system. The six elements and the dependency rule survive intact; what
needs translation is the shape of an interface, where the composition
root lives, and how much the compiler actually enforces.

Examples use the original guide's domain (users, organizations,
profiles, a database). Familiarity with Haskell is assumed; familiarity
with `effectful` is not. Two facts about it suffice:

- `Eff es a` is the monad. `es` is a type-level list of effects, and the
  constraint `e :> es` reads "`e` is available in `es`".
- `IOE :> es` means arbitrary `IO` is available. Treat it exactly as the
  guide treats a `Db` parameter: **authority over everything**.

Confidence markers as in the original. **[standard]** is established
practice; **[synthesis]** is assembled from standard parts;
**[untested]** follows from the model but has not been built.

---

## 1. The mapping

| Guide element | Haskell realization |
|---|---|
| Resource | A concrete value (`TVar`, connection pool), built in `main`, never in a core signature |
| Capability | Record of functions, parameterized by the effect row: `data ProfileEditor es` |
| Surface | The same, returning capabilities: `data UserSurface es` |
| Driven adapter | A function `mkUserSurface :: Db -> UserSurface es` |
| Driving adapter | HTTP handler, worker loop — whatever the framework dispatches |
| Composition root | `main`, plus one `runReader` wrapping the application |
| Core | Plain functions taking capabilities; **no `IOE` in the signature** |

The dependency rule is enforced by module imports and export lists, both
greppable. *[standard]*

---

## 2. Capabilities are records of functions over the effect row

The guide's TypeScript interface becomes a record whose fields are
effectful actions:

```haskell
{-# LANGUAGE NoFieldSelectors #-}

data ProfileEditor es = ProfileEditor
  { changeDisplayName :: Text -> Eff es ()
  , changeBio         :: Text -> Eff es ()
  }

data SelfProfileEditor es = SelfProfileEditor
  { editor         :: ProfileEditor es
  , changeEmail    :: Text -> Eff es ()
  , changePassword :: Text -> Eff es ()
  }
```

The `es` parameter is what lets one value be used from any call site. The
guide's rules carry over unchanged: the object is fixed at construction,
never passed to a method, and a method may take an ID only if it returns
a narrower reference rather than performing an effect.

With `NoFieldSelectors` plus `OverloadedRecordDot`, call sites read
almost exactly like the guide's TypeScript:

```haskell
editor.changeDisplayName trimmed
```

`NoFieldSelectors` also suppresses the top-level selector functions, so
`.field` becomes the *only* way to invoke a capability method — no prefix
form to reach for by accident.

Two mechanical notes, both easy to trip over:

- Record-dot resolution needs the field **name** in scope in the calling
  module, like any other identifier. It does not arrive automatically
  with the type.
- Under `NoFieldSelectors` there is no bare value to import, so the name
  must come in through the type: `import M (ProfileEditor (changeBio))`.

**One-method capability?** The guide says make it a function; in Haskell
that is simply `type ProfileReader es = Eff es Profile`, and the advice
applies with even less friction. *[standard]*

---

## 3. Branding is free

The guide warns that TypeScript's structural typing makes
`interface AdminProfileEditor extends ProfileEditor {}` indistinguishable
from its parent, requiring a `unique symbol` hack to force a real
distinction.

Haskell is nominally typed, so this problem does not exist. A `newtype`
over the same record is a genuinely different type, unusable where the
other is expected, with no phantom field and no runtime representation:

```haskell
newtype AdminProfileEditor es = AdminProfileEditor (ProfileEditor es)
```

Same-shape-different-provenance is one `newtype`. *[standard]*

---

## 4. The composition root: one record of adapters behind `Reader`

`main` builds the resources and assembles one record of adapters, which
is injected as a single `Reader` effect:

```haskell
newtype Adapters = Adapters
  { getUsers :: forall es. (IOE :> es) => Eff es (UserSurface es)
  }

main :: IO ()
main = do
  db <- connect config.database
  let adapters = Adapters { getUsers = pure (mkUserSurface db) }
  runApp (runReader adapters application)
```

Handlers then carry `Reader Adapters :> es` instead of a pile of
independently-constrained imports, and reach the surface through one
helper:

```haskell
usersSurface :: (Reader Adapters :> es, IOE :> es) => Eff es (UserSurface es)
usersSurface = do
  adapters <- ask @Adapters
  getUsers adapters          -- prefix application; see below

-- at a call site
users <- usersSurface
profile <- users.profileOf userId
```

Three non-obvious things about that field type, each of which cost a
compile cycle to discover:

**The `forall es.` must be inside the field.** The tempting alternative
is to parameterize the record instead — `data Adapters es` with a plain
`getUsers :: Eff es (UserSurface es)` field. Handler signatures using it
compile fine; the constraint `Reader (Adapters es) :> es` is perfectly
legal, and it is *not* an occurs check. It fails later, at discharge:
`runReader` needs an `Adapters` whose own row already contains the
`Reader` that holds it — whose row contains the `Reader` holding that,
and so on. GHC reports the regress rather than the cause:

```
There is no handler for 'Reader (Adapters [Reader (Adapters es0), IOE])'
```

Quantifying inside the field keeps `Adapters` an ordinary monomorphic
type, safe to put behind a `Reader`, with each read instantiating fresh.
This is reinforced if your framework dispatches handlers at rows of its
own choosing (adding a layer per handler), since then one value must
serve many different rows regardless.

**Record-dot cannot project that field.** `adapters.getUsers` does not
compile: `HasField` has no instance for a field whose *declared* type is
quantified. This is a property of the field, not of the use site — it
fails under both inference and checking. The auto-generated selector
*function* is merely top-level-polymorphic and instantiates by ordinary
application, so `getUsers adapters` works. Confine that one prefix call
to the projection helper; everything downstream is concrete and dots
normally.

**Wrap the result in `Eff`.** A bare `UserSurface es` field would force
the projection into continuation-passing style —
`withUsers $ \users -> ...` — nesting every handler's entire body inside
a lambda. Returning `Eff es (UserSurface es)` makes the projection a
plain bind. This is purely ergonomic; it does not make the field
record-dot-projectable.

Finally: prefer `Effectful.Reader.Static` over the dynamic variant
unless you need to reinterpret at runtime — `effectful`'s own docs
recommend the static one as the default. *[synthesis]*

---

## 5. Enforcement: what Haskell actually buys

The guide's closing limitation is that TypeScript cannot stop a module
from importing an adapter and constructing one, so the model "buys
discipline, not enforcement."

Haskell does better on that specific point. Keep the composition-root
record's constructor out of the export list and it becomes genuinely
unforgeable outside its module:

```haskell
module App.Adapters (Adapters, newAdapters, usersSurface) where
--                   ^ type only, no (..)
```

Holding `Reader Adapters :> es` then grants exactly the operations that
module chose to export — never the underlying pool or `TVar`, which
cannot be pattern-matched back out. The guide's suggested mitigations
(unexported constructors, one factory per module) stop being conventions
and become compile errors.

**But the guarantee is only as strong as your effect rows.** A core
function carrying `IOE :> es` can do anything, capability or no
capability — it is precisely the guide's "core function takes a `Db`"
failure mode wearing a different hat. The discipline that matters in
Haskell is keeping `IOE` out of core signatures, so that a core function
whose row lists no `IOE` genuinely *cannot* reach the outside world
except through the capabilities it was handed. That property is real,
checked, and worth protecting. *[synthesis]*

---

## 6. Separated Interface: three modules, not two

Fowler's Separated Interface (cited in the guide's further reading) turns
out to be worth following literally, because it changes what must be
exported:

- **`App.Domain`** — plain data (`Profile`, `Membership`). Ordinary field
  selectors; this is read everywhere and there is nothing to protect.
- **`App.Capability`** — `UserSurface`, `ProfileEditor` as interfaces
  only. No `Db`, no `IO`. **Constructors exported**: building an adapter,
  real or fake, is the one thing outsiders should do with these types.
- **`App.Adapters`** — the concrete implementation over real resources,
  plus the composition-root record. **Constructor unexported.**

The asymmetry is the point. Exporting the *interface* constructor costs
nothing — a hand-built `UserSurface` has whatever authority its own
fields close over, which for a test fake is none. Exporting the
*composition root's* constructor would hand out the real database.
*[standard]*

---

## 7. When to reach for a dynamic effect instead

An obvious question in an effect-system codebase: why not make the
surface itself an effect, rather than a record behind `Reader`?

```haskell
data Users :: Effect where
  GetUsers :: Users m (UserSurface m)
type instance DispatchOf Users = Dynamic
```

The dividing line is how each mechanism identifies its target. An effect
is resolved by **type**, one per row, statically. That fits a Surface —
singular, one per process, known at compile time. It does not fit a
Capability scoped to a runtime-chosen identity: "the editor for user 7"
and "the editor for user 9" would have to be the same type, and there is
no sensible type-level tag per user ID. So at most the top-level surface
becomes an effect; capabilities stay ordinary returned values, however
deep the chain goes.

Even for the surface, there is a trap worth knowing before you start.
The declaration above makes `Users` a **higher-order effect**, because
the operation's result mentions the effect's own `m` parameter.
Interpreting one requires the `localUnlift` family, and that machinery
exists to *run* an `m`-typed action handed in as an argument — not to
*construct* fresh ones targeting a caller row the interpreter cannot see.
Concretely, `EffectHandler` guarantees only `e :> localEs` about that
row; nothing lets you build an `IOE`-requiring surface at it. The fix is
to pin the surface's methods to plain `IO`, which makes the effect
first-order and interpretation trivial — at the cost of a `liftIO` at
every single call site, and of a surface that can no longer be
implemented in terms of other effects.

We built this, measured the cost, and reverted to the `Reader` record.
For a surface with one implementation per process it is a bad trade; the
record keeps methods row-polymorphic and needs no lifting. It would
become attractive if you genuinely wanted to swap interpretations at
runtime, or to stack interpreters (logging, retry) around the surface.

There is also a plainer argument: two dependency-injection mechanisms in
one codebase is itself a cost. A record of handles is easier to follow
than an effect for readers who know Haskell but not `effectful`.
*[untested]* as a general recommendation — this is one data point, at one
scale, with one surface.

---

## 8. Testing

The guide says to hand-write a fake capability, and that they are small
because the capabilities are small. In Haskell a fake is a record
literal:

```haskell
fakeEditor :: IORef Profile -> ProfileEditor es
fakeEditor ref = ProfileEditor
  { changeDisplayName = \name -> modifyIORef ref (\p -> p { displayName = name })
  , changeBio         = \bio  -> modifyIORef ref (\p -> p { bio = bio })
  }
```

No mocking framework, no interface duplication, no DI container. This is
the main practical payoff of `App.Capability` being importable without
`App.Adapters`: a test can construct the fake without linking the
database layer at all. *[standard]*

---

## 9. Failure modes (addendum to the guide's table)

| Symptom | Problem |
|---|---|
| Module-level `unsafePerformIO` resource | Ambient authority. Build it in `main`, thread it as an effect. |
| Core function carrying `IOE :> es` | Authority over everything — the guide's "takes a `Db`" in disguise. |
| `Adapters es` parameterized by the row | Typechecks, but can never be discharged: the value's own row would have to contain the `Reader` holding it. Quantify inside the field. |
| Record-dot on a rank-2 field | `HasField` needs a non-quantified declared type. Use prefix application, once. |
| CPS wrapper around every handler body | Return `Eff es (Surface es)` and bind, rather than a bare surface. |
| Capability method takes an ID and performs an effect | Same as the guide: put the ID in the constructor. |
| `liftIO` appearing throughout the core | The surface got pinned to `IO` — usually the higher-order-effect trap in §7. |

---

## 10. What did not change

Everything in the guide about *sizing* interfaces. The four coordinates,
narrowing at the earliest point you know a value, capabilities being
cheap and short-lived, surfaces having no effectful methods, creation
designating the container, driving adapters owning their wire types —
none of that is language-specific, and none of it needed adjustment.

The Haskell-specific content above is entirely about *plumbing*: how to
get an interface's worth of functions from `main` to a handler without
losing polymorphism or gaining ambient authority. That is a smaller
problem than sizing the interfaces correctly, and it is worth being
clear-eyed that solving it well does not substitute for doing that.
