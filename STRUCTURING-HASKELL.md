# Structuring an Application in Haskell

A variant of Ports and Adapters where the interfaces the business logic sees are sized by how much power they grant.

Examples are Haskell using `effectful`, and describe one system: users, organizations, memberships, profiles, and a database. Two facts about `effectful` are enough to read them:

- `Eff es a` is the monad. `es` is a type-level list of effects, and `e :> es` reads "`e` is available in `es`".
- `IOE :> es` means arbitrary `IO` is available. Treat it exactly as this guide treats a `Db` argument: **authority over everything**.

Confidence markers: **[standard]** is established practice. **[synthesis]** is assembled from standard parts. **[untested]** follows from the model but has not been built.

---

## 1. The six elements

| Element | What it is |
|---|---|
| **Resource** | Concrete value holding state that outlives the adapters using it: connection pool, HTTP client, queue producer, a long-lived cache. |
| **Capability** | Small record of functions granting specific powers over a specific object. |
| **Surface** | Wide, shallow record covering one type of thing. Hands out capabilities; does nothing itself. |
| **Adapter** | Concrete implementation. **Driven** adapters build capabilities and surfaces from resources. **Driving** adapters (HTTP handlers, consumers, jobs) call the core. |
| **Composition Root** | Where resources are built and wired. |
| **Core** | Business logic. Takes capabilities as arguments and has no other way to reach the outside world. |

The dependency rule: the core depends on capabilities and surfaces; everything else depends on the core.

The rule underneath: **the reference is the permission.** Code holding a capability can use it; code that doesn't, can't. There is no separate permission check, because failing to obtain the value is the denial.

For this to mean anything the core must have no other route out. In Haskell that reduces to one rule with teeth:

> **No `IOE` in a core signature.**

A core function whose row contains no `IOE` cannot reach the outside world except through the capabilities it was handed. That is checked by the compiler, and it is the property everything else here protects.

**Isn't this dependency injection?** Mechanically yes. The difference is how the arguments are sized: the smallest thing that does the job, not a whole service.

*[standard] — Ports and Adapters, Clean/Onion Architecture, object-capability discipline.*

---

## 2. Resources

```haskell
-- App.Resources

data Db = Db
  { query  :: forall a. FromRow a => Query -> [SqlValue] -> IO [a]
  , exec   :: Query -> [SqlValue] -> IO ()
  , withTx :: forall a. (Db -> IO a) -> IO a
  }

connectDb :: DbConfig -> (Db -> IO a) -> IO a
```

Connection strings, pools, retries and timeouts live here and nowhere else. The methods are in `IO`: a resource is the boundary where `IO` is legitimate.

**What makes something a Resource is lifetime, not externality.** Most adapters are built per request and discarded with it; a few, where the application has a session concept, are built per user session and kept for a while. Anything whose state must outlive the adapters using it has to be created outside them and handed in — and that is a Resource whether it talks to a network or lives entirely in memory. State that fits inside one adapter and dies with it is just that adapter's internal state and needs no modelling at all.

1. Built once, in the composition root.
2. Never in a core signature. A core function holding a `Db` has authority over the whole database.
3. Explicit acquisition and release, recorded in one place.

**Is a cache a Resource?** Ask who has to see it. A cache shared across requests is one, in memory or not — being an `IORef` rather than a Redis client changes nothing about the fact that something must own it and hand it out. A cache serving one adapter for one request isn't: keep it inside the adapter.

**Configuration?** Depends on whether it changes.

*Static config* is data. Read and validate it once at startup, then pass values into constructors. It never appears again as a thing to consult.

*Mutable config* — anything an ops endpoint, a feature-flag service or a `SIGHUP` can change while the process runs — is a Resource. It's stateful and something writes to it, which means reading it is an effect and its value can differ between two reads in the same function. Core code must therefore see it as a capability (`Eff es RateLimits`, say), not as a plain argument. The endpoint that updates it is a driving adapter, and the capability that lets it write should be as narrow as any other.

*[standard]*

---

## 3. Capabilities and Surfaces

### Authority has four coordinates

The authority a value grants is a set of tuples:

**(subject, verb, object type, object instance)**

Every capability constrains some coordinates and leaves others free.

### Capability

Constrains all four: this actor, these verbs, this object. A capability is a record of functions, parameterized by the effect row:

```haskell
-- App.Capability

newtype ProfileReader es = ProfileReader
  { readProfile :: Eff es Profile
  }

data ProfileEditor es = ProfileEditor
  { changeDisplayName :: Text -> Eff es ()
  , changeBio         :: Text -> Eff es ()
  }

-- Only the owner may change credentials.
data SelfProfileEditor es = SelfProfileEditor
  { editor         :: ProfileEditor es
  , changeEmail    :: Email -> Eff es ()
  , changePassword :: Password -> Eff es ()
  }
```

No user ID, no org ID in any field type. The object is fixed when the capability is constructed.

An org admin editing someone else's profile gets a plain `ProfileEditor`; the owner gets a `SelfProfileEditor`. Same object, different verbs. Restricting what you may do narrows authority as much as restricting what you may do it to.

The `es` parameter is what lets one value be used from any call site. It is not a constraint on the capability — it's how the capability stays usable wherever the caller happens to be.

`ProfileReader` has one operation and is still a `newtype` rather than a synonym. A synonym would make every `Eff es Profile` a `ProfileReader`, which throws away the thing capabilities are for: the type is supposed to say what authority this value carries, not just what it returns. A `newtype` keeps that nominal at no runtime cost, and leaves room for a second verb later.

### Surface

Constrains the object type, leaves the instance free.

```haskell
-- App.Capability

data UserSurface es = UserSurface
  { forSelf   :: AuthenticatedUser -> SelfProfileEditor es
  , profileOf :: UserId -> Eff es (Maybe (ProfileReader es))
  }

data OrgSurface es = OrgSurface
  { forOrg :: OrgId -> Eff es (Maybe (OrgContext es))
  }
```

`forSelf` takes the authenticated actor rather than reading it from somewhere ambient: the subject is a coordinate like any other and should be named where it's fixed. Its type is `AuthenticatedUser`, produced only by authentication — a bare `UserId` would let anyone ask for anyone's editor.

A Surface lets code say "I work with users and organizations and nothing else." That is a large gain in comprehension — most of the domain becomes unnameable — and a small one in authority, since a Surface still reaches every user.

These are two different benefits. Narrowing the object type shrinks vocabulary. Narrowing subject, verb or instance shrinks what can happen.

### Surfaces have no effectful methods

```haskell
-- Wrong
data UserSurface es = UserSurface
  { changeEmail :: UserId -> Email -> Eff es ()
  }
```

Holding that value limits nothing. The scope check has moved into a conditional inside the implementation, and a conditional can be dropped in a refactor without the type changing.

```haskell
-- Right
let self = forSelf users (actor session)   -- authority fixed once
changeEmail self newEmail                  -- no ID to get wrong
```

### Design rule

Fix each coordinate at the earliest point where you know it, and hand the core the narrowest bundle you can build. A core function given an org ID plus a broad value can reach every organization and relies on itself not to.

### Capabilities are cheap and short-lived

Build them, use them, discard them. Don't store them, don't reuse them across requests, and don't cache them — a capability closes over who derived it and why, so sharing one is how an admin-derived editor ends up serving a self-service request.

Building one usually involves a permission check, so if those checks are expensive the adapter can cache the underlying facts for the duration of a request. That is the adapter's business and shouldn't appear in the capability types.

**Surface or Capability?** Default to Capability. Use a Surface only when the caller genuinely looks up many instances of one type: search, listing, bulk import.

**Isn't a pile of tiny records its own problem?** Yes, if you split per verb. Group the verbs one caller needs together — `ProfileEditor` has two fields, not two types.

**Can a Capability field take an ID?** Only if it returns a narrower capability rather than performing an effect. `profileEditorFor admin userId` is fine: it checks containment once and returns something scoped. `changeEmailOf admin userId email` is not.

**That still relies on a check.** Once, at construction, producing a value you then hold — rather than a check you can forget to make.

*[standard] for least authority, designation-as-authorization, and the subject/object matrix. [synthesis] for the Surface/Capability pair and the four coordinates.*

---

## 4. Adapters

### Driven

Build capabilities and surfaces from resources. Where SQL lives.

```haskell
-- App.Adapters.Db

mkProfileEditor :: (IOE :> es) => Db -> UserId -> ProfileEditor es
mkProfileEditor db userId = ProfileEditor
  { changeDisplayName = \name -> liftIO $
      exec db "update profiles set display_name = ? where user_id = ?"
              [toSql name, toSql userId]
  , changeBio = \bio -> liftIO $
      exec db "update profiles set bio = ? where user_id = ?"
              [toSql bio, toSql userId]
  }

mkSelfProfileEditor :: (IOE :> es) => Db -> UserId -> SelfProfileEditor es
mkSelfProfileEditor db userId = SelfProfileEditor
  { editor         = mkProfileEditor db userId
  , changeEmail    = \email -> liftIO $ exec db "..." [toSql email, toSql userId]
  , changePassword = \pw    -> liftIO $ exec db "..." [toSql pw, toSql userId]
  }
```

The user ID is a constructor argument, not a field argument. That is what makes it a capability.

The `IOE :> es` constraint lives *here*, on the adapter, and nowhere upstream of it. Each closed-over action carries its own authority; the resulting `ProfileEditor es` can be handed to a core function whose row has no `IOE` at all, and that function still cannot do anything but call these two fields.

Surfaces are built from a session and construct the leaves themselves, so resources stay on the adapter side:

```haskell
mkUserSurface :: (IOE :> es) => RequestSession -> UserSurface es
mkUserSurface session = UserSurface
  { forSelf = \actor -> mkSelfProfileEditor (db session) (userId actor)
  , profileOf = \uid -> do
      rows <- liftIO $ query (db session) "select 1 from users where id = ?" [toSql uid]
      pure $ if null (rows :: [Only Int])
               then Nothing
               else Just (mkProfileReader (db session) uid)
  }
```

Driven adapters come in **families**: sets of coupled constructors building several capability and surface types over one shared set of resources. `mkUserSurface`, `mkOrgSurface` and `mkSelfProfileEditor` are one family over `Db`. The family is the unit you replace when you change how something is stored.

An adapter may use more than one resource — writing to the database and publishing to a queue in one field is normal. Only the core is restricted to capabilities.

### Driving

HTTP handlers, consumers, jobs. They implement nothing the core defines.

```haskell
-- App.Http.Profile
import qualified App.Capability as Cap
import qualified App.Core.Profile as Core

handleChangeDisplayName
  :: (Reader Adapters :> es, Error DomainError :> es, IOE :> es)
  => RequestSession -> Text -> Eff es Response
handleChangeDisplayName session name = do
  users <- usersSurface session
  let self = Cap.forSelf users (actor session)
  Core.changeDisplayName (Cap.editor self) name
  pure noContent
```

Do not define a class or record for driving adapters. Only the driven side needs inversion.

### Two rules for the edge

1. Driving adapters own their wire types. Deriving `FromJSON`/`ToJSON` on domain types makes the wire format a consumer of the domain, so internal refactors become breaking API changes.
2. Mapping is the adapter's job in both directions: driven adapters map rows, driving adapters map JSON.

**Where do status codes live?** The driving adapter. The core returns a result or throws a domain error.

*[standard]*

---

## 5. The Core

Plain functions taking everything they need as arguments, with no `IOE` in the row.

```haskell
-- App.Core.Profile
import qualified App.Capability as Cap

data DomainError
  = ValidationError Text
  | QuotaExceeded Int
  deriving stock (Show)

changeDisplayName
  :: (Error DomainError :> es)
  => Cap.ProfileEditor es -> Text -> Eff es ()
changeDisplayName editor name = do
  let trimmed = Text.strip name
  when (Text.length trimmed < 2)  $ throwError (ValidationError "too short")
  when (Text.length trimmed > 50) $ throwError (ValidationError "too long")
  Cap.changeDisplayName editor trimmed
```

1. No ambient access, enforced by the absence of `IOE`.
2. Take capabilities — not resources, and not IDs plus resources.
3. Don't wrap core functions in a class. There is nothing to substitute.

**What a legitimate core row looks like.** It is not empty. `changeDisplayName` needs `Error DomainError`, and a core function might reasonably also want `State`, `NonDet`, or a domain-specific effect of your own. What it must not contain is `IOE`. Everything the function does to the outside world goes through the capability, whose actions were closed over by an adapter that *did* have `IOE`; everything else is a domain effect the caller discharges. So the rule is not "core functions are polymorphic in `es`" — it's that the row lists domain effects and never `IOE`.

### Pure logic and orchestration

Some core functions take no capabilities. They are fully determined by their inputs and need no setup to test.

```haskell
-- App.Core.Membership

canPromote :: Membership -> Membership -> Bool
canPromote actor target = role actor == Admin && role target == Member
```

Others call capabilities, branch on results, and call more capabilities.

**Separate the two into layers?** Maybe. Extract the pure part when:

- the fragment has a name a domain expert would recognise — if it doesn't, keep it inline in the effectful function;
- you want to property-test it; or
- the branch count is high enough that enumerating cases beats constructing fakes.

The test for whether an extraction is real: **can you name the function and state its contract without mentioning its caller?** `canPromote` passes. `validateStep2` does not — that isn't a unit of logic, it's a line of the caller moved somewhere less convenient.

The cost of not splitting: it is always locally cheaper to reach for an injected capability than to thread data out and back, so the effectful share of the code tends to grow. The type system won't catch this — the function gains a parameter and still compiles.

### More than one capability

Most core functions take several capabilities and decide between them:

```haskell
-- App.Core.Membership

inviteMember
  :: (Error DomainError :> es)
  => Cap.SeatCounter es -> Cap.MemberInviter es -> Email -> Eff es Invitation
inviteMember seats inviter email = do
  s <- Cap.readSeats seats
  when (used s >= limit s) $ throwError (QuotaExceeded (limit s))
  Cap.invite inviter email Member
```

Both capabilities are already scoped to one organization, so the function has no organization ID and can't act on the wrong one. What it decides is which of the two to use.

**Those two calls need to be atomic — where does the transaction go?** Not in the core, which has no vocabulary for transactions and shouldn't acquire one. Two options, in order of preference:

1. **Put the effects behind one capability field.** If a check and a write must not race, they are one operation, and `invite` should do both inside its own transaction. The core makes the decision it cares about; the adapter guarantees atomicity.
2. **Build the whole capability family inside a transaction.** Since capabilities close over the resource, running `withTx` at the driving adapter and swapping the handle in the session yields a family whose actions all run on the transactional connection:

```haskell
handleInvite session email = liftIO $ withTx (db session) $ \tx ->
  runRequest session { db = tx } $ do
    orgs <- orgsSurface ...
    ...                        -- every capability built here shares the transaction
```

Reach for the first whenever the atomic unit has a name. Use the second when it doesn't. Note this is why the session carries the `Db` rather than the adapters closing over a process-level one: the swap has to be possible.

**May a core function narrow further?** Yes. Rule 2 in §7 is about where the *authorization chain* is established, not a ban on capability getters inside the core. A core function holding an `OrgAdminContext` may call `inviter` on it — it isn't gaining authority, only naming part of what it already has.

**Logic needs data it might not use — pass a loader?** Often fine. The criterion is legibility, not purity. A nullary read-only loader — `Eff es Seats` — usually reads clearly. Loaders taking arguments are a warning sign, and one that modifies state means the function isn't pure and you should stop pretending it is.

**Keep it pure by returning a plan for the caller to interpret?** Usually not worth it. Tests then assert on the plan rather than the outcome, so they couple to internal shape, and the interpretation code isn't covered by them. A straight function with a fake capability tests better.

*[standard] for the dependency rule. [synthesis] for the extraction criteria.*

---

## 6. The Composition Root

There are two, and separating them matters: process-level authority is fixed at deployment, request-level authority depends on who is asking.

**Static root (`main`)** — builds resources, chooses the adapter family, starts driving adapters. Runs once.

**Dynamic root (per request)** — opens a session, authenticates, derives capabilities. Runs in the driving adapter.

```haskell
-- App.Adapters.Wiring
module App.Adapters.Wiring
  ( Adapters                    -- type only; no (..)
  , productionAdapters
  , usersSurface
  , orgsSurface
  , RequestSession (..)
  ) where

data Adapters = Adapters
  { getUsers :: forall es. (IOE :> es) => RequestSession -> Eff es (UserSurface es)
  , getOrgs  :: forall es. (IOE :> es) => RequestSession -> Eff es (OrgSurface es)
  }

productionAdapters :: Adapters
productionAdapters = Adapters
  { getUsers = pure . mkUserSurface
  , getOrgs  = pure . mkOrgSurface
  }

usersSurface :: (Reader Adapters :> es, IOE :> es)
             => RequestSession -> Eff es (UserSurface es)
usersSurface session = do
  adapters <- ask @Adapters
  getUsers adapters session
```

`productionAdapters` takes no arguments because the session already carries the `Db` — which it must, so that §5's transaction swap is possible. The record still earns its place: it is the seam where a test substitutes a whole family, the same way a per-environment system map does.

```haskell
-- App.Main
main :: IO ()
main = connectDb (database config) $ \db ->
  runEff . runReader productionAdapters $ do
    schedule <- startSchedule (mkReconciler db) config.schedule
    server   <- startHttpServer router config.port
    untilShutdownSignal
      `finally` (stopServer server >> stopSchedule schedule)
```

Handlers then carry `Reader Adapters :> es` instead of a pile of independently-threaded arguments.

**Quantify `forall es.` inside the field, not over the record.** The tempting alternative is `data Adapters es` with a plain `getUsers :: RequestSession -> Eff es (UserSurface es)`. Handler signatures using it compile; `Reader (Adapters es) :> es` is legal and is *not* an occurs check. It fails later, at discharge: `runReader` needs an `Adapters` whose own row already contains the `Reader` holding it — whose row contains the `Reader` holding that, without end. GHC reports the regress, not the cause:

```
There is no handler for 'Reader (Adapters [Reader (Adapters es0), IOE])'
```

Quantifying inside the field keeps `Adapters` monomorphic and safe to put behind a `Reader`, with each read instantiating fresh.

**Return `Eff es (Surface es)`, not a bare surface.** Constructing a surface is often itself effectful — a pool checkout, a cache lookup — and a monadic field keeps that possible without changing every call site later. It also makes the projection a plain bind rather than something you have to fit into a `let`.

Prefer `Effectful.Reader.Static` unless you need to reinterpret at runtime; the static variant is the default recommendation and has no dispatch cost.

### The session

```haskell
data RequestSession = RequestSession
  { db    :: Db
  , actor :: AuthenticatedUser
  }
```

Adapter families are constructed from a session, so everything they build is scoped to one request and one actor without any of that being threaded through by hand.

A `RequestSession` exposes its resources, which would violate Rule 3 in §7 if it were a capability. It isn't one: it's a composition-root construct, held only by adapters, never passed to the core. Keep it out of core signatures and the distinction holds.

*[standard] for the composition root. [synthesis] for the static/dynamic split and the rank-2 record.*

---

## 7. The narrowing chain

```haskell
-- App.Http.AdminProfile
import qualified App.Capability as Cap
import qualified App.Core.Profile as Core

handleAdminEditProfile
  :: (Reader Adapters :> es, Error DomainError :> es, IOE :> es)
  => RequestSession -> OrgId -> UserId -> Text -> Eff es Response
handleAdminEditProfile session orgId targetId newName = do
  orgs <- orgsSurface session
  result <- runExceptT $ do
    org    <- orElse (status 404) $ Cap.forOrg orgs orgId
    admin  <- orElse (status 403) $ Cap.asAdmin org
    editor <- orElse (status 404) $ Cap.profileEditorFor admin targetId
    lift $ Core.changeDisplayName editor newName
  pure $ either id (const noContent) result
  where
    orElse err act = lift act >>= maybe (throwE err) pure
```

Each step converts a check into a value. The `Nothing` cases are the authorization.

By the last step, `editor` is a `ProfileEditor` fixed to one user in one organization, with no field for changing an email or password.

The three failures get different statuses, and the reasons differ:

- `forOrg` returns 404 to a non-member, because telling them the organization exists is already a leak.
- `asAdmin` returns 403. The caller has just proved they can see this organization, so there is nothing left to conceal, and 404 would only make the error worse.
- `profileEditorFor` returns 404 for both "no such user" and "not in this organization", collapsing the two so that membership of other organizations stays hidden.

`ExceptT` carrying the status is the plumbing; the point is the sequence of narrowings.

**Rule 1 — name every coordinate you fix.** `asAdmin org` is right because `forOrg` already fixed the organization. A hypothetical `asOrganizationAdmin me` is wrong: it either assumes single membership or picks one ambiently, and the call site can't tell which.

**Rule 2 — narrow in the driving adapter, act in the core.** The chain is per-request and effectful, so it belongs at the edge. The last step must hand off. A handler that narrows and then calls the capability field directly has skipped validation entirely.

**Rule 3 — no generic way back up.** No `parent` field, no exposed raw resource. Widening is sometimes legitimate and domain-specific — a support tool escalating with a recorded reason, say — but it should be an explicit, checked operation rather than a property every capability has.

**Collapse "not found" and "not permitted" where there is existence to hide.** That is the case for `profileEditorFor` above, and not for `asAdmin`, whose caller already knows the organization exists. Apply the rule step by step rather than to the whole chain: for public objects it's needless everywhere, and a collapse that hides nothing costs you a useful error.

**Can I inspect a capability to see what it permits?** No. Invoke it. A record of functions has no `Eq`, no `Ord` and no useful `Show`, which is convenient: the language won't let you treat authority as data even if you try.

**Then how do I grey out UI buttons?** Attempt to build one, and discard it.

```haskell
permissionsFor :: (Reader Adapters :> es, IOE :> es)
               => RequestSession -> OrgId -> Eff es Permissions
permissionsFor session orgId = do
  orgs   <- orgsSurface session
  mOrg   <- Cap.forOrg orgs orgId
  mAdmin <- maybe (pure Nothing) Cap.asAdmin mOrg
  pure Permissions
    { canView       = isJust mOrg
    , canAdminister = isJust mAdmin
    }
```

The *decision* can't drift, because this is the same code path as enforcement — unlike a parallel `can user action` function, which is a second implementation of the rule. The *enumeration* still can: add a permission and forget to list it here, and the UI silently omits it.

*[standard] for attenuated facets and revocation wrappers. [synthesis] for the rules.*

---

## 8. Creation and system-level actions

Things are created inside a container, and the container is the value that grants the right to create:

```haskell
data MemberInviter es = MemberInviter
  { invite :: Email -> Role -> Eff es Invitation
  }

data OrgAdminContext es = OrgAdminContext
  { profileEditorFor :: UserId -> Eff es (Maybe (ProfileEditor es))
  , inviter          :: MemberInviter es
  , seats            :: SeatCounter es
  }
```

Holding an `OrgAdminContext` is the right to invite into that organization. No org ID is passed, so none can be wrong.

### System-level actions

What's left is genuine system authority: reconciliation jobs, provisioning handlers, migrations.

Don't build a `SystemSurface`. Derive system authority per deployment instead, hand-wiring each job its own slice:

```haskell
-- App.Main
let reconciler = mkOrgReconciler
      (mkOrgLister db)        -- read all orgs
      (mkSeatAdjuster db)     -- adjust seat counts
                              -- and nothing else
```

Keep these behind a module that exports the factories and not the constructors, so "nothing outside the wiring can obtain system authority" is a compile error rather than a review convention.

Where the work is event-driven, build per event with the event closed over, so records show which occurrence caused the change.

*[standard] for creation designating the container. [untested] for the system-authority prescription.*

---

## 9. Modules, testing, and what the compiler enforces

### Three modules, not two

Fowler's Separated Interface is worth following literally here, because it changes what must be exported:

- **`App.Domain`** — plain data (`Profile`, `Membership`). Ordinary field selectors; this is read everywhere and there is nothing to protect.
- **`App.Capability`** — `UserSurface`, `ProfileEditor` and friends, as types only. No `Db`, no `IO`. **Constructors exported**: building one, real or fake, is the one thing outsiders should do with these types.
- **`App.Adapters.*`** — concrete implementations over real resources, plus the wiring record. **Constructors unexported**; factories exported.

The asymmetry is the point. Exporting a *capability* constructor costs nothing: a hand-built `ProfileEditor` has whatever authority its own fields close over, which for a test fake is none. Exporting the *wiring* constructor would hand out the real database.

### Import records qualified

Record fields generate top-level selectors in the ordinary namespace, so any module defining records is a namespace hazard — `name`, `id` and `read` collide almost immediately, and a core function usually wants the same name as the capability field it wraps. Import them qualified and the problem disappears without prefixing every field:

```haskell
import qualified App.Capability as Cap
import qualified App.Core.Profile as Core
```

There's a real cost at *construction* sites, because record syntax needs the field labels in scope too: an adapter would have to write `Cap.ProfileEditor { Cap.changeDisplayName = ... }`. Since adapters don't define competing names, import unqualified there:

```haskell
-- App.Adapters.Db
import App.Capability (ProfileEditor (..), SelfProfileEditor (..))
```

Qualified where capabilities are *used*, unqualified where they are *built*. It also puts the layer boundary in the call site — `Cap.changeDisplayName` next to `Core.changeDisplayName` says which side of the architecture you're on.

### Testing

A fake is a record literal:

```haskell
fakeEditor :: (IOE :> es) => IORef Profile -> ProfileEditor es
fakeEditor ref = ProfileEditor
  { changeDisplayName = \name -> liftIO $ modifyIORef ref $ \p -> p { displayName = name }
  , changeBio         = \bio  -> liftIO $ modifyIORef ref $ \p -> p { bio = bio }
  }
```

No mocking framework, no interface duplication. This is the practical payoff of `App.Capability` being importable without `App.Adapters`: a test constructs the fake without linking the database layer at all.

### What is actually enforced

Keep the wiring record's constructor unexported and it becomes unforgeable outside its module. Holding `Reader Adapters :> es` then grants exactly the operations that module chose to export — never the underlying pool, which cannot be pattern-matched back out.

But the guarantee is only as strong as your effect rows. A core function carrying `IOE :> es` can do anything, capability or not. Everything above rests on that one rule, so it is worth a lint or a review habit: **`IOE` appears in adapters, in `main`, and nowhere else.**

*[standard] for the module split and record-literal fakes. [synthesis] for the enforcement summary.*

---

## 10. Putting it together

One request:

```
HTTP request
  └─ handleAdminEditProfile     (Reader Adapters, Error DomainError, IOE)
       ├─ orgsSurface session            → OrgSurface es
       ├─ forOrg orgs orgId              → Maybe (OrgContext es)       [404]
       ├─ asAdmin org                    → Maybe (OrgAdminContext es)  [403]
       ├─ profileEditorFor admin userId  → Maybe (ProfileEditor es)    [404]
       ├─ Core.changeDisplayName editor name      -- Error only, no IOE
       │    └─ Cap.changeDisplayName editor trimmed
       └─ (session ends with the handler)
```

### Scaffolding order

1. `App.Domain`: plain data. Nouns only.
2. Resource types, one per external system, with one constructor each.
3. `App.Capability`: capability records, grouped by the verbs one caller needs. Object closed over at construction, never a field argument.
4. Surfaces, only where a caller looks up many instances. Capability getters only.
5. The narrowing chain for the primary use case: which coordinate each step fixes, and what `Nothing` means.
6. Core functions. Capabilities in; domain effects in the row; no `IOE`.
7. Driven adapters.
8. Driving adapters, with their own wire types and status codes.
9. The wiring record and `main`.

### Failure modes

| Symptom | Problem |
|---|---|
| Core function carries `IOE :> es` | Authority over everything. Take capabilities instead. |
| Module-level `unsafePerformIO` resource | Ambient authority. Build it in `main` and thread it. |
| A core function takes a `Db` | Same failure with the constraint spelled out. |
| Capability field takes an ID and performs an effect | Scope check moved into a conditional. Close over the ID at construction. |
| `data Adapters es` parameterized by the row | Compiles, never discharges: the value's row would have to contain the `Reader` holding it. Quantify inside the field. |
| `liftIO` scattered through the core | A capability got pinned to `IO`. Keep capability fields in `Eff es`. |
| A `System` or `Manager` record with dozens of fields | Residue of things hard to scope. Hand-wire slices instead. |
| Capabilities stored or reused across requests | They close over who derived them. Build fresh ones. |
| `FromJSON` on a domain type | Wire format consumes the domain. Give the adapter its own types. |
| A parallel `can user action` function | Second implementation of the rule; it will drift. |
| Pure fragments that only make sense in sequence | Extracted too little, or shouldn't have extracted. |

### Limitation

Two things remain conventions rather than guarantees.

Nothing stops a core module from adding `IOE :> es` to its own signature. The property is checkable but not self-enforcing, which is why it belongs in review.

And a capability is only as narrow as the closure inside it. `mkProfileEditor db userId` could have ignored `userId` and updated every row; the type would not have changed. The compiler guarantees the *core* cannot exceed what it was handed. It cannot guarantee the adapter handed over what it claimed.

---

## 11. Further reading

**Architecture.** Cockburn, "Hexagonal Architecture" (2005) — the original, including the driving/driven distinction. Martin, *Clean Architecture* — the dependency rule. Nubank's "diplomat architecture" — a large Clojure instance with a named layer for effectful glue; note their vocabulary inverts Cockburn's, using "port" for concrete infrastructure.

**Handles and wiring.** Jasper Van der Jeugt, "The Handle Pattern" — records of functions as the unit of dependency injection in Haskell, which is what §3's capabilities are. Matt Parsons, "The Three Layer Haskell Cake" — the same stratification as §5, with `ReaderT` as the transport. Seemann, *Dependency Injection Principles, Practices, and Patterns* — the composition root.

**Core and glue.** Bernhardt, "Boundaries" — functional core, imperative shell, the stricter position this guide declines. Parnas (1972) — why decomposing along process steps produces modules you can't understand independently. Ousterhout, *A Philosophy of Software Design* — deep versus shallow modules. Haxl — what to reach for when pure logic needs data-dependent fetching.

**Capabilities.** Dennis and Van Horn (1966) and Mark Miller's work — the origin, and the distinction between permission (what you can invoke) and authority (what you can eventually cause). Hardy, "The Confused Deputy" (1988) — why broad authority plus a caller-supplied ID is a bug shape. Lampson, "Protection" (1971) — the access matrix. Capsicum and `openat` — capability discipline in C, and the everyday case of creation designating a container. seL4 — derivation trees and subtree revocation.

**Interface shapes.** Fowler, *Patterns of Enterprise Application Architecture* — Header Interface for the Surface shape, Separated Interface for the module split in §9.

**Grounding this in an authorization store.** Zanzibar and its descendants (SpiceDB, OpenFGA) are a ready-made foundation. Zanzibar's record is `namespace:object#relation@subject` — the same four coordinates as §3. SpiceDB's schema separates stored relations from computed permissions, so you can generate one capability type per (object type, permission), each constructor being a check returning `Maybe`, with the permission expressions supplying the narrowing order.

Two caveats. Surfaces have no representation there — a tuple needs a concrete object ID, so the type-scoped, instance-free shape stays hand-written. And these are decision services, not effect systems: a generated capability is a check with a hole where the data access goes.

---

## Appendix: why capabilities aren't effects

An obvious question in an `effectful` codebase: why not make a Surface an effect, rather than a record behind `Reader`?

```haskell
data Users :: Effect where
  GetUsers :: Users m (UserSurface m)
type instance DispatchOf Users = Dynamic
```

The dividing line is how each mechanism identifies its target. **An effect is resolved by type, statically. A capability is scoped to a runtime-chosen instance.** That is the object-type versus object-instance coordinate from §3, and it falls exactly on Haskell's type/value boundary: "the editor for user 7" and "the editor for user 9" would have to be the same type, and there is no sensible type-level tag per user ID. (`Labeled` does allow several instances of one effect, but labels are static too, so it doesn't change the argument.)

So at most a Surface can be an effect. Capabilities stay ordinary returned values, however deep the chain goes.

Even for a Surface there is a trap. The declaration above is a **higher-order effect**, because the operation's result mentions the effect's own `m`. Interpreting one needs the `localUnlift` family, and that machinery exists to *run* an `m`-typed action handed in as an argument — not to *construct* fresh ones targeting a caller row the interpreter cannot see. `EffectHandler` guarantees only `e :> localEs` about that row; nothing lets you build an `IOE`-requiring surface at it. The fix is to pin the surface's methods to plain `IO`, which makes the effect first-order and interpretation trivial — at the cost of a `liftIO` at every call site, and a surface that can no longer be implemented in terms of other effects.

For a surface with one implementation per process this is a bad trade, and the `Reader` record in §6 is the better default: methods stay row-polymorphic and need no lifting. An effect becomes attractive if you genuinely want to swap interpretations at runtime, or to stack interpreters — logging, retry, tracing — around the surface.

There is also a plainer argument: two dependency-injection mechanisms in one codebase is itself a cost, and a record of functions is easier to follow than an effect for readers who know Haskell but not `effectful`. *[untested] as a general recommendation — one data point, at one scale, with one surface.*
