package sample1.domain.cta

import sample1.domain.entity.EntityRepo
import sample1.domain.errors.CtaError

trait CtaRepo[F[_]] extends EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaError]
