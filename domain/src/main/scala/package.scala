package pear

import matryoshka.data.Mu
import matryoshka.patterns.EnvT

package object domain {
  type QuestionF[A] = EnvT[FormMetadata, form.Definition.FormF, A]

  type Question = Mu[QuestionF]
}
