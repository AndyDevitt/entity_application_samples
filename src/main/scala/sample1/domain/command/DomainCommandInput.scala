package sample1.domain.command

import sample1.domain.cta.CtaRepo
import sample1.domain.invoice.InvoiceRepo
import sample1.domain.invoice.commands.ExampleDomainService.ExampleDomainService

class DomainCommandInput[F[_]](val invoiceRepo: InvoiceRepo[F],
                               val ctaRepo: CtaRepo[F],
                               val service: ExampleDomainService[F]
                              ) extends CommandInput
