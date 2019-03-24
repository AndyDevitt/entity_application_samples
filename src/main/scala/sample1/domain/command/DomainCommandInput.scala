package sample1.domain.command

import sample1.domain.cta.CtaRepo
import sample1.domain.invoice.InvoiceRepo

class DomainCommandInput[F[_]](val invoiceRepo: InvoiceRepo[F],
                               val ctaRepo: CtaRepo[F]
                              ) extends CommandInput
