package sample1.application

import cats.data.EitherT
import cats.effect.IO
import cats.instances.future._
import cats.{Id, ~>}
import sample1.domain._
import sample1.domain.command._
import sample1.domain.cta.ClinicalTrialAgreement
import sample1.domain.entity.{EntityRepoCodec, EntityVersion, Versioned}
import sample1.domain.invoice.{Invoice, SiteInvoice, SponsorInvoice}
import sample1.infrastructure.{ProductionInvoiceRepo, TestCtaRepo, TestInvoiceRepo}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}

object TestImplicits {

  /**
    * Natural transformation from an IO context to a Future
    */
  implicit val ioToFutureTransform: IO ~> Future = new ~>[IO, Future] {
    override def apply[A](fa: IO[A]): Future[A] = fa.unsafeToFuture()
  }

  /**
    * Natural transform instance generator for two monads of the same type
    */
  implicit def sameContextTransform[F[_]]: F ~> F = new ~>[F, F] {
    override def apply[A](fa: F[A]): F[A] = fa
  }

  /**
    * Versioned instances to support the version increment in the repo.
    */
  implicit val invoiceVersioned: Versioned[Invoice] = Versioned.instance {
    case i: SiteInvoice => i.copy(version = i.version.nextVersion)
    case i: SponsorInvoice => i.copy(version = i.version.nextVersion)
  }

  implicit val ctaVersioned: Versioned[ClinicalTrialAgreement] =
    Versioned.instance(cta => cta.copy(version = cta.version.nextVersion))

  /**
    * Application layer transformer that enables the client of the application to inject transformers from the domain
    * types into an appropriate type to be consumed by the client. This instance enables conversion between an Invoice
    * and an InvoiceView. The client can then simply specify the generic types on the call to process command to enable
    * a conversion different from the default.
    */
  implicit val invoiceToViewTransformer: ApplicationTransformer[InvoiceView, Invoice, InvoiceError] =
    (b: Invoice) => InvoiceView.create(b)

  /**
    * CTA codec used to encode and decode between domain and persistence types (both the same type in this example) This
    * is where a business process transformation may be injected (i.e. between the domain repo and persistence repo
    * layers). Hence the encode must always succeed, but the decode may fail.
    */
  implicit val ctaRepoCodec: EntityRepoCodec[ClinicalTrialAgreement, ClinicalTrialAgreement, InvoiceError] =
    EntityRepoCodec.instance[ClinicalTrialAgreement, ClinicalTrialAgreement, InvoiceError](
      (cta: ClinicalTrialAgreement) => cta,
      (cta: ClinicalTrialAgreement) => Right(cta.copy(note = "I've been flipped!")))
}

object ApplicationTestsV5 extends App {

  import TestImplicits._

  //implicit val idUpdateRunner = CommandRunner.invoiceUpdateCommandRunner[Id, Id]
  //implicit val idCreateRunner = CommandRunner.invoiceCreateCommandRunner[Id, Id]
  //implicit val idRetrieveRunner = CommandRunner.invoiceRetrieveCommandRunner[Id, Id]
  //implicit val idCreateRfiRunner = CommandRunner.invoiceCreateRfiCommandRunner[Id, Id]

  val user1 = UserId("User1")
  val user2 = UserId("User2")

  val prodApp = new ProdApplication(new ProductionInvoiceRepo())
  val testApp = new TestApplication(new TestInvoiceRepo())

  // TODO [AD]: consider naming of the application transformer and the repo codec and also their current inheritance
  //  structure (i.e. deriving from the Codec family of traits). Also, should these simply be explicit parameters?
  val testProcessorApp = new TestApplicationWithProcessor(new TestInvoiceRepo(), new TestCtaRepo())

  val res = (for {
    inv <- EitherT(prodApp.createRfiInvoice(CreateRfiInvoiceCmd(user1)))
    updated <- EitherT(prodApp.approveInvoice(ApproveCmd(user2, inv.id, inv.version)))
  } yield updated).value

  res.onComplete {
    case Success(value) => println(s"Result: ${value.toString}")
    case Failure(exception) => println(s"Failure: ${exception.toString}")
  }

  Await.result(res, 10 seconds)

  val res2 = for {
    inv1 <- testApp.createRfiInvoice(CreateRfiInvoiceCmd(user1))
    inv2 <- testApp.approveInvoice(ApproveCmd(user2, inv1.id, inv1.version))
  } yield inv2

  println(s"res2: $res2")

  val res3 = for {
    inv1 <- testApp.createRfiInvoice(CreateRfiInvoiceCmd(user1))
    inv2 <- testApp.approveInvoiceAndTrans(ApproveCmd(user2, inv1.id, EntityVersion()))
  } yield inv2

  println(s"res3: $res3")

  val res4 = for {
    inv1 <- testApp.createRfiInvoice(CreateRfiInvoiceCmd(user1))
    inv2 <- testApp.approveInvoice2(ApproveCmd(user2, inv1.id, inv1.version))
  } yield inv2

  println(s"res4: $res4")

  val res5 = for {
    inv1 <- testApp.createRfiInvoice(CreateRfiInvoiceCmd(user1))
    inv2 <- testApp.updateRfi(UpdateRfiCmd(user2, inv1.id, inv1.version))
  } yield inv2

  println(s"res5: $res5")

  val res6 = for {
    inv1 <- testApp.createRfiInvoice(CreateRfiInvoiceCmd(user1))
    inv2 <- testApp.approveInvoice2(ApproveCmd(user2, inv1.id, inv1.version))
    inv3 <- testApp.updateRfi(UpdateRfiCmd(user1, inv2.id, inv2.version))
  } yield inv3

  println(s"res6: $res6")

  val res7 = for {
    inv1 <- testApp.createRfiInvoice(CreateRfiInvoiceCmd(user1))
    inv2 <- testApp.approveInvoiceAndTrans(ApproveCmd(user2, inv1.id, inv1.version))
  } yield inv2

  println(s"res7: $res7")

  val res8 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmdG(user1))
    inv2 <- testProcessorApp.processCommand(ApproveCmdG(user2, inv1.id, inv1.version))
  } yield inv2

  println(s"res8: $res8")

  val res9 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmdG(user1))
    inv2 <- testProcessorApp.processCommand(ApproveCmdG(user2, inv1.id, inv1.version))
  } yield inv2

  println(s"res9: $res9")

  val res10 = for {
    inv1 <- testProcessorApp.processCommand[Invoice, Invoice](CreateRfiInvoiceCmdG(user1))
    inv2 <- testProcessorApp.processCommand[Invoice, InvoiceView](ApproveCmdG(user2, inv1.id, inv1.version))
  } yield inv2

  println(s"res10: $res10")

  val res11 = for {
    inv <- testProcessorApp.processCommand[Invoice, Invoice](CreateRfiInvoiceCmdG(user1))
    invRetrieved <- testProcessorApp.processCommand[Invoice, Invoice](InvoiceRetrieveCommandG(user1, inv.id))
  } yield invRetrieved

  println(s"res11: $res11")

  val res12 = for {
    cta <- testProcessorApp.processCommand(CreateCtaCmdG(user1))
  } yield cta

  println(s"res12: $res12")

  val res13 = for {
    cta <- testProcessorApp.processCommand(CreateCtaCmdG(user1))
    ctaRetrieved <- testProcessorApp.processCommand(CtaRetrieveCommandG(user1, cta.id))
  } yield ctaRetrieved

  println(s"res13: $res13")

  val res14 = for {
    inv <- testProcessorApp.processCommandX[Invoice, Invoice, CreateRfiInvoiceCmdG[Id]](CreateRfiInvoiceCmdG(user1))
    invRetrieved <- testProcessorApp.processCommandX[Invoice, InvoiceView, InvoiceRetrieveCommandG[Id]](InvoiceRetrieveCommandG(user1, inv.id))
  } yield invRetrieved

  println(s"res14: $res14")
}
