package sample1.domain.cta

import sample1.domain.InvoiceError
import sample1.domain.entity.EntityRepo

trait CtaRepo[F[_]] extends EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError]