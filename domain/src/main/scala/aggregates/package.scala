package pear
package domain

import scalaz.{NonEmptyList, State}

package object aggregates {

  type Handler[E, S]         = E => State[S, Unit]
  type CreationHandler[E, S] = E => S
  def todo[A] = (a: A) => a

  val ProposalConstructor: CreationHandler[ProposalCreated, Proposal] = { e =>
    Proposal.init(e.target, e.author)
  }

  val ProposalAggregate: Handler[ProposalEvent, Proposal] = { e =>
    State.modify {
      e match {
        case ProposalDrafted(_, content)     => Proposal.content.set(Some(content))
        case ProposalSaved(_, author)        => Proposal.speakers.set(NonEmptyList.nels(author))
        case ProposalCancelled(_)            => todo
        case ProposalAcceptedByCommittee(_)  => todo
        case ProposalRefusedByCommittee(_)   => todo
        case UserVoted(_, user, vote)        => Proposal.votes.modify(_ + (user -> vote))
        case ProposalPutInWaitList(_)        => todo
        case ProposalSelectedFromWaitList(_) => todo
        case MessagePosted(_, message)       => Proposal.messages.modify(message +: _)
      }
    }
  }

  val TalkAggregate: Handler[TalkEvent, Talk] = { e =>
    State.modify {
      e match {
        case TalkConfirmedBySpeaker(_, _) => Talk.status.set(Unscheduled)
        case TalkDeclinedBySpeaker(_, _)  => Talk.status.set(Cancelled)
        case TalkScheduled(_, slot)       => Talk.status.set(Scheduled(slot))
        case TalkUnscheduled(_)           => Talk.status.set(Unscheduled)
      }
    }
  }

  val SpeakerAggregate: Handler[SpeakerEvent, Speaker] = { e =>
    State.modify {
      e match {
        case SpeakerInformationSaved(_, info)           => Speaker.personnalInformation.set(info)
        case SpeakerExpenseInformationSaved(_, expense) => Speaker.expensesCovering.set(expense)
      }
    }
  }
}
