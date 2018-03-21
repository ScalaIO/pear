package pear
package domain

import monocle.macros.Lenses
import aggregates._

@Lenses
final case class Store[I, T, E <: Event[I, T]](data: Map[I, T])

object Store {
  def handleEvent[I, T, E <: Event[I, T]](evt: E, store: Store[I, T, E], handler: Handler[E, T]): Store[I, T, E] =
    data.modify(
      (d: Map[I, T]) =>
        d.get(evt.target)
          .map(entity => d.updated(evt.target, handler(evt).exec(entity)))
          .getOrElse(d))(store)

  def handleCreationEvent[I, T, E <: CreationEvent[I, T]](evt: E,
                                                          store: Store[I, T, E],
                                                          handler: CreationHandler[E, T]): Store[I, T, E] =
    data.modify(
      (d: Map[I, T]) => d + (evt.target -> handler(evt))
    )(store)
}

@Lenses
final case class Model(proposals: Store[ProposalId, Proposal, ProposalEvent],
                       talks: Store[TalkId, Talk, TalkEvent],
                       speakers: Store[SpeakerId, Speaker, SpeakerEvent])

object Model {
  def handleEvent[I, T, E <: Event[I, T]](event: E, model: Model): Model = event match {
    case e: ProposalEvent => proposals.modify(p => Store.handleEvent(e, p, ProposalAggregate))(model)
    case e: TalkEvent     => talks.modify(t => Store.handleEvent(e, t, TalkAggregate))(model)
    case e: SpeakerEvent  => speakers.modify(s => Store.handleEvent(e, s, SpeakerAggregate))(model)
    case _: UserEvent     => model
    case e                => throw new IllegalArgumentException(s"unknown event type $e")
  }
}
