package pear
package domain

sealed trait Event[Id, Entity] {
  def target: Id
}

sealed trait CreationEvent[Id, Entity] extends Event[Id, Entity]

// User
sealed trait UserEvent extends Event[UserId, UserAccount]

final case class AccountCreated(target: UserId) extends CreationEvent[UserId, UserAccount]

// Proposal
sealed trait ProposalEvent extends Event[ProposalId, Proposal]

final case class ProposalCreated(target: ProposalId, author: SpeakerId) extends CreationEvent[ProposalId, Proposal]

final case class ProposalDrafted(target: ProposalId, content: Answer)    extends ProposalEvent
final case class ProposalSaved(target: ProposalId, author: SpeakerId)    extends ProposalEvent
final case class ProposalCancelled(target: ProposalId)                   extends ProposalEvent
final case class ProposalAcceptedByCommittee(target: ProposalId)         extends ProposalEvent
final case class ProposalRefusedByCommittee(target: ProposalId)          extends ProposalEvent
final case class UserVoted(target: ProposalId, user: UserId, vote: Vote) extends ProposalEvent
final case class ProposalPutInWaitList(target: ProposalId)               extends ProposalEvent
final case class ProposalSelectedFromWaitList(target: ProposalId)        extends ProposalEvent
final case class MessagePosted(target: ProposalId, message: Message)     extends ProposalEvent
// Talk
sealed trait TalkEvent                                                      extends Event[TalkId, Talk]
final case class TalkConfirmedBySpeaker(target: TalkId, speaker: SpeakerId) extends TalkEvent
final case class TalkDeclinedBySpeaker(target: TalkId, speaker: SpeakerId)  extends TalkEvent
final case class TalkScheduled(target: TalkId, slot: TimeSlot)              extends TalkEvent
final case class TalkUnscheduled(target: TalkId)                            extends TalkEvent

final case class InvitedTalkCreated(target: TalkId) extends CreationEvent[TalkId, Talk]

// Speaker
sealed trait SpeakerEvent                                                                 extends Event[SpeakerId, Speaker]
final case class SpeakerCreated(target: SpeakerId)                                        extends CreationEvent[SpeakerId, Speaker]
final case class SpeakerInformationSaved(target: SpeakerId, personnalInformation: Answer) extends SpeakerEvent
final case class SpeakerExpenseInformationSaved(target: SpeakerId, expenseInfo: Answer)   extends SpeakerEvent
