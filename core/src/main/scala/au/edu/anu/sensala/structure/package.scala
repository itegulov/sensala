package au.edu.anu.sensala

import cats.data.State

package object structure {
  // TODO: Perhaps we should eventually reuse Scavenger's data structures for logical expressions
  // TODO: extend this with articles, NL quantifiers, adverbs, ...

  // State monad over the Context type aliases
  type ContextState[A] = State[Context, A]
  type CState          = State[Context, L]
}
