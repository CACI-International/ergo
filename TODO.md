* Extend the Ctrl-C behavior to better cancel/stop execution.
  * Right now it only cancels tasks.
* Persist command timing information for better estimates.
* Add value dependency tree print to help debug consistency issues.
* Debugger and profiler.
* Dropping thread for Values (to avoid possibly large stacks).
* Improve cache reads (batch them).
* Figure out how to add backtraces.
* Invalidate and/or remove cache entries.
* Possibly don't evaluate eval_for_id values within Unbound bodies.
  * This would just have fewer surprises, and some things would "just work" like
    using `ergo` or `std:dynamic:eval` within function bodies (as opposed to
    `std:ergo-lazy`, or adding `id ~eval=Bool:false`). The price would be that an Unbound's
    identity would be only based on captures (so e.g. it would rely on `std`
    rather than `std:fs:write` if `std:fs:write` appears in the body).
  * Semantically, one could consider `eval_for_id` as acting on the "immediate"
    data tree (where Unbounds need input to produce the value).
  * A bit of work would be needed to still allow Unbound to inherit eval_for_id
    in a logical way.
* Possibly figure out a way for late bindings to propagate through certain
  function calls (e.g. `ergo abc` should "just work" if late bound).
