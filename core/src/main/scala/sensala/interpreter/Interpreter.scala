package sensala.interpreter

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.{All, Ex}
import sensala.error.{NLInvalidState, NLUnexpectedWord}
import sensala.structure._
import sensala.interpreter.context.{Context, LocalContext}
import sensala.structure.types.{entity, event}

final case class Interpreter[F[_]: Monad: Context: LocalContext: FunctorRaiseNLError]() {
  def interpret(nl: NL, cont: F[E]): F[E] =
    nl match {
      case Sentence(nounPhrase, verbPhrase) =>
        interpret(nounPhrase, interpret(verbPhrase, cont))
      case Discourse(sentences) =>
        for {
          x <- Context[F].bindFreeVar
          result <- sentences.foldLeftM[F, E](Abs(x, i, x)) {
                     case (e, b) =>
                       for {
                         z      <- Context[F].bindFreeVar
                         _      <- LocalContext[F].clear
                         intRes <- interpret(b, Monad[F].pure[E](z))
                       } yield Abs(z, entity, e(intRes))
                   }
          contL <- cont
        } yield result(contL)
      case WhNounPhrase(verbPhrase, nounPhrase) =>
        interpret(
          nounPhrase,
          for {
            x <- LocalContext[F].getEntity
            vpL <- interpret(
                    verbPhrase,
                    for {
                      _ <- LocalContext[F]
                            .putEntity(x) // Because who clause can redefine current entity
                      contL <- cont
                    } yield contL
                  )
          } yield vpL
        )
      case IntransitiveVerb(word) =>
        for {
          x     <- LocalContext[F].getEntity
          e     <- Context[F].bindFreeVar
          _     <- LocalContext[F].putEvent(e)
          w     = Sym(word)
          _     <- Context[F].addEvent(e, w(e) /\ agent(e, x))
          contL <- cont
        } yield Ex(e, event, w(e) /\ agent(e, x) /\ contL)
      case TransitiveVerb(word, obj) =>
        for {
          x <- LocalContext[F].getEntity
          e <- Context[F].bindFreeVar
          _ <- LocalContext[F].putEvent(e)
          w = Sym(word)
          objL <- interpret(
                   obj,
                   for {
                     y     <- LocalContext[F].getEntity
                     _     <- Context[F].addEvent(e, w(e) /\ agent(e, x) /\ patient(e, y))
                     contL <- cont
                   } yield Ex(e, event, w(e) /\ agent(e, x) /\ patient(e, y) /\ contL)
                 )
        } yield objL
      case VerbAdjectivePhrase(verb, adjective) =>
        verb match {
          case "is" | "was" =>
            for {
              x     <- LocalContext[F].getEntity
              e     <- Context[F].bindFreeVar
              _     <- LocalContext[F].putEvent(e)
              w     = Sym(adjective.word)
              _     <- Context[F].addEvent(e, description(e) /\ w(e, x))
              contL <- cont
            } yield Ex(e, event, description(e) /\ w(e, x) /\ contL)
          case other =>
            FunctorRaiseNLError[F].raise(NLUnexpectedWord(other))
        }
      case VerbInPhrase(propositionalPhrase, verbPhrase) =>
        interpret(
          verbPhrase,
          for {
            e <- LocalContext[F].getEvent
            locationL <- interpret(
                          propositionalPhrase.nounPhrase,
                          for {
                            x <- LocalContext[F].getEntity
                            w = Sym(propositionalPhrase.verbWord)
                            properties <- Context[F].eventProperties(e) >>= {
                                           case All(`e`, `event`, body) =>
                                             Monad[F].pure[E](body)
                                           case _ =>
                                             FunctorRaiseNLError[F].raise[E](
                                               NLInvalidState("Unexpected properties format")
                                             )
                                         }
                            _     <- Context[F].addEvent(e, properties /\ w(e, x))
                            contL <- cont
                          } yield w(e, x) /\ contL
                        )
          } yield locationL
        )
      case VerbPhraseAnaphora(phrase, voice) =>
        for {
          e <- Context[F].findAnaphoricEventUnsafe(List.empty)
          properties <- Context[F].eventProperties(e) >>= {
                         case All(`e`, `event`, body) =>
                           Monad[F].pure[E](body)
                         case _ =>
                           FunctorRaiseNLError[F].raise[E](
                             NLInvalidState("Unexpected properties format")
                           )
                       }
          entity <- LocalContext[F].getEntity
          newE   <- Context[F].bindFreeVar
          newProperties = voice match {
            case Active  => substitute(substitute(properties, e, newE), agent, 1, entity)
            case Passive => substitute(substitute(properties, e, newE), patient, 1, entity)
          }
          _     <- LocalContext[F].putEvent(newE)
          _     <- Context[F].addEvent(newE, newProperties)
          contL <- cont
        } yield Ex(newE, event, newProperties /\ contL)
      case VerbSentencePhrase(word, sentence) =>
        for {
          e <- Context[F].bindFreeVar
          x <- LocalContext[F].getEntity
          _ <- LocalContext[F].putEvent(e)
          w = Sym(word)
          sentenceL <- interpret(
                        sentence,
                        for {
                          eSucc <- LocalContext[F].getEvent
                          _     <- Context[F].addEvent(e, w(e) /\ agent(e, x) /\ patient(e, eSucc))
                          contL <- cont
                        } yield w(e) /\ agent(e, x) /\ patient(e, eSucc) /\ contL
                      )
        } yield Ex(e, event, sentenceL)
      case InPhrase(verbWord, nounPhrase) =>
        Monad[F].pure[E](Sym(verbWord))
      case possessionPhrase: PossessionPhrase =>
        Monad[F].pure[E](Sym(possessionPhrase.verbWord))
      case ProperNoun(word, _, _) =>
        for {
          x     <- LocalContext[F].getEntity
          w     = Sym(word)
          contL <- cont
        } yield named(x, w) /\ contL
      case CommonNoun(word) =>
        for {
          x     <- LocalContext[F].getEntity
          w     = Sym(word)
          contL <- cont
        } yield w(x) /\ contL
      case NounPhrasePreposition(prepositionalPhrase, nounPhrase) =>
        interpret(
          nounPhrase,
          for {
            e <- LocalContext[F].getEntity
            preposition <- interpret(
                            prepositionalPhrase.nounPhrase,
                            for {
                              prepEntity <- LocalContext[F].getEntity
                              w          = Sym(prepositionalPhrase.verbWord)
                              _          <- LocalContext[F].putEntity(e)
                              contL      <- cont
                            } yield w(prepEntity, e) /\ contL
                          )
          } yield preposition
        )
      case quantifier @ ForallQuantifier(nounPhrase) =>
        for {
          x     <- Context[F].bindFreeVar
          _     <- Context[F].addEntity(x, quantifier.properties)
          _     <- LocalContext[F].putEntity(x)
          nounL <- interpret(nounPhrase, cont.map(~_))
        } yield All(x, entity, ~nounL)
      case quantifier @ ExistentialQuantifier(nounPhrase) =>
        for {
          x     <- Context[F].bindFreeVar
          _     <- Context[F].addEntity(x, quantifier.properties)
          _     <- LocalContext[F].putEntity(x)
          nounL <- interpret(nounPhrase, cont)
        } yield Ex(x, entity, nounL)
      case DefiniteNounPhrase(nounPhrase) =>
        for {
          refOpt <- Context[F].findAnaphoricEntity(nounPhrase.definiteProperties)
          result <- refOpt match {
                     case Some(ref) =>
                       for {
                         _     <- LocalContext[F].putEntity(ref)
                         contL <- cont
                       } yield contL
                     case None =>
                       for {
                         x     <- Context[F].bindFreeVar
                         _     <- Context[F].addEntity(x, nounPhrase.properties)
                         _     <- LocalContext[F].putEntity(x)
                         nounL <- interpret(nounPhrase, cont)
                       } yield Ex(x, entity, nounL)
                   }
        } yield result
      case DemonstrativePronoun(word) =>
        for {
          e     <- Context[F].findAnaphoricEventUnsafe(List.empty)
          _     <- LocalContext[F].putEntity(e)
          contL <- cont
        } yield contL
      case pronoun: NegativePersonSingularIndefinitePronoun =>
        for {
          x     <- Context[F].bindFreeVar
          _     <- Context[F].addEntity(x, pronoun.properties)
          _     <- LocalContext[F].putEntity(x)
          contL <- cont
        } yield ~Ex(x, entity, contL)
      case pronoun: UniversalPersonSingularIndefinitePronoun =>
        for {
          x     <- Context[F].bindFreeVar
          _     <- Context[F].addEntity(x, pronoun.properties)
          _     <- LocalContext[F].putEntity(x)
          contL <- cont
        } yield All(x, entity, contL)
      case pronoun: ExistentialPersonSingularIndefinitePronoun =>
        for {
          x     <- Context[F].bindFreeVar
          _     <- Context[F].addEntity(x, pronoun.properties)
          _     <- LocalContext[F].putEntity(x)
          contL <- cont
        } yield Ex(x, entity, contL)
      case pronoun: NegativeThingSingularIndefinitePronoun =>
        for {
          x     <- Context[F].bindFreeVar
          _     <- Context[F].addEntity(x, pronoun.properties)
          _     <- LocalContext[F].putEntity(x)
          contL <- cont
        } yield ~Ex(x, entity, contL)
      case pronoun: UniversalThingSingularIndefinitePronoun =>
        for {
          x     <- Context[F].bindFreeVar
          _     <- Context[F].addEntity(x, pronoun.properties)
          _     <- LocalContext[F].putEntity(x)
          contL <- cont
        } yield All(x, entity, contL)
      case pronoun: ExistentialThingSingularIndefinitePronoun =>
        for {
          x     <- Context[F].bindFreeVar
          _     <- Context[F].addEntity(x, pronoun.properties)
          _     <- LocalContext[F].putEntity(x)
          contL <- cont
        } yield Ex(x, entity, contL)
      case pronoun: PersonalPronoun =>
        for {
          ref   <- Context[F].findAnaphoricEntityUnsafe(pronoun.properties)
          _     <- LocalContext[F].putEntity(ref)
          contL <- cont
        } yield contL
      case pronoun: PossessivePronoun =>
        for {
          ref   <- Context[F].findAnaphoricEntityUnsafe(pronoun.properties)
          _     <- LocalContext[F].putEntity(ref)
          contL <- cont
        } yield contL
      case pronoun: ReflexivePronoun =>
        for {
          ref   <- Context[F].findAnaphoricEntityUnsafe(pronoun.properties)
          _     <- LocalContext[F].putEntity(ref)
          contL <- cont
        } yield contL
      case VerbAdverbPhrase(adverb, verbPhrase) =>
        interpret(
          verbPhrase,
          for {
            e <- LocalContext[F].getEvent
            w = Sym(adverb.word)
            properties <- Context[F].eventProperties(e) >>= {
                           case All(`e`, `event`, body) =>
                             Monad[F].pure[E](body)
                           case _ =>
                             FunctorRaiseNLError[F]
                               .raise[E](NLInvalidState("Unexpected properties format"))
                         }
            _     <- Context[F].addEvent(e, properties /\ w(e))
            contL <- cont
          } yield w(e) /\ contL
        )
      case AdjectiveNounPhrase(adjective, nounPhrase) =>
        interpret(
          nounPhrase,
          for {
            y     <- LocalContext[F].getEntity
            w     = Sym(adjective.word)
            contL <- cont
          } yield w(y) /\ contL
        )
    }
}
