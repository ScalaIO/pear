package pear
package domain

import java.time.ZonedDateTime
import java.util.{Locale, UUID}
import scala.concurrent.duration.FiniteDuration
import scalaz.NonEmptyList
import monocle.macros.Lenses

// identities
final case class FormId(uuid: UUID)     extends AnyVal
final case class DraftId(uuid: UUID)    extends AnyVal
final case class ProposalId(uuid: UUID) extends AnyVal
final case class TalkId(uuid: UUID)     extends AnyVal
final case class SpeakerId(uuid: UUID)  extends AnyVal
final case class UserId(uuid: UUID)     extends AnyVal

object Id {
  import shapeless._
  def random[T](implicit gen: Generic.Aux[T, UUID :: HNil]): T = gen.from(UUID.randomUUID() :: HNil)
}

// value objects
final case class RenderingMetadata()
final case class FormMetadata(label: Map[Locale, String], rendering: RenderingMetadata)

final case class Form(id: FormId, question: Question)

@Lenses
final case class Answer(formId: FormId, version: Int, value: form.FormValue)

sealed trait Vote

@Lenses
final case class Conversation(messages: Vector[Message])

object Conversation {
  def empty = Conversation(Vector.empty)
}

final case class Message(text: String, date: ZonedDateTime, author: UserId)

sealed trait TalkStatus
case object AwaitingConfirmation           extends TalkStatus
case object Unscheduled                    extends TalkStatus
case object Cancelled                      extends TalkStatus
final case class Scheduled(slot: TimeSlot) extends TalkStatus

@Lenses
final case class TimeSlot(room: Room, start: ZonedDateTime, duration: FiniteDuration)

@Lenses
final case class Room(name: String)

@Lenses
final case class Venue(name: String, address: Address)

@Lenses
final case class Address(street: String, city: String, country: String)

// aggregates
@Lenses
final case class Draft(id: DraftId = Id.random[DraftId], speakers: NonEmptyList[SpeakerId], content: Answer)

@Lenses
final case class Proposal(id: ProposalId = Id.random[ProposalId],
                          speakers: NonEmptyList[SpeakerId],
                          content: Option[Answer] = None,
                          votes: Map[UserId, Vote] = Map.empty,
                          conversation: Conversation = Conversation.empty)

object Proposal {
  val messages = Proposal.conversation composeLens Conversation.messages

  def init(proposalId: ProposalId, author: SpeakerId) = Proposal(proposalId, NonEmptyList.nels(author))
}

@Lenses
final case class Talk(id: TalkId = Id.random[TalkId],
                      status: TalkStatus = Unscheduled,
                      speakers: NonEmptyList[SpeakerId])

@Lenses
final case class Speaker(speakerId: SpeakerId, personnalInformation: Answer, expensesCovering: Answer)

@Lenses
final case class UserAccount(userId: UserId)
