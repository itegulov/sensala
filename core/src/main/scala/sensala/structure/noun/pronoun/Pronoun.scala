package sensala.structure.noun.pronoun

import sensala.structure.Word
import sensala.structure.noun.NounPhrase

trait Pronoun[F[_]] extends NounPhrase[F] with Word[F]
