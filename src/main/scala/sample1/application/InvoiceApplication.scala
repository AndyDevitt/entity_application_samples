package sample1.application

import cats.Id
import cats.effect.IO
import sample1.domain.command._
import sample1.domain.cta.CtaRepo
import sample1.domain.{InvoiceError, InvoiceRepo}

import scala.concurrent.Future

/**
  * Invoice Application which extends EntityApplication and simply needs to define the input type and construct it
  *
  * @tparam F the context for the application
  * @tparam G the context for the underlying repo
  */
trait InvoiceApplicationWithProcessor[F[_], G[_]] extends EntityApplication[F, G, DomainCommandInput[G], InvoiceError] {
  def invoiceRepo: InvoiceRepo[G]

  def ctaRepo: CtaRepo[G]

  override final val input: DomainCommandInput[G] = new DomainCommandInput[G](invoiceRepo, ctaRepo)
}

/**
  * An example production application using Future and IO contexts for the application and repo respectively
  *
  * @param invoiceRepo
  * @param ctaRepo
  */
class ProdApplicationWithProcessor(override val invoiceRepo: InvoiceRepo[IO], override val ctaRepo: CtaRepo[IO]) extends InvoiceApplicationWithProcessor[Future, IO]

/**
  * An example test repo that simply uses the Id context
  *
  * @param invoiceRepo
  * @param ctaRepo
  */
class TestApplicationWithProcessor(override val invoiceRepo: InvoiceRepo[Id], override val ctaRepo: CtaRepo[Id]) extends InvoiceApplicationWithProcessor[Id, Id]
