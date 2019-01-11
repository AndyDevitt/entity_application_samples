package sample1.application

import cats.data.EitherT
import cats.effect.IO
import cats.instances.future._
import cats.~>
import sample1.domain._
import sample1.domain.command._
import sample1.domain.cta.ClinicalTrialAgreement
import sample1.domain.entity.{EntityVersion, Versioned}
import sample1.domain.invoice.{Invoice, SiteInvoice, SponsorInvoice}
import sample1.infrastructure.{ProductionInvoiceRepo, TestCtaRepo, TestInvoiceRepo}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}

object TestImplicits {

  implicit val ioToFutureTransform: IO ~> Future = new ~>[IO, Future] {
    override def apply[A](fa: IO[A]): Future[A] = fa.unsafeToFuture()
  }

  implicit def sameContextTransform[F[_]]: F ~> F = new ~>[F, F] {
    override def apply[A](fa: F[A]): F[A] = fa
  }

  implicit val invoiceVersioned: Versioned[Invoice] = Versioned.instance {
    case i: SiteInvoice => i.copy(version = i.version.nextVersion)
    case i: SponsorInvoice => i.copy(version = i.version.nextVersion)
  }

  implicit val ctaVersioned: Versioned[ClinicalTrialAgreement] =
    Versioned.instance(cta => cta.copy(version = cta.version.nextVersion))

  implicit val invoiceToViewDecoder: Decoder[InvoiceView, Invoice, InvoiceError] =
    (b: Invoice) => InvoiceView.create(b)

  val ctaRepoCodec: Codec[ClinicalTrialAgreement, ClinicalTrialAgreement, InvoiceError] =
    Codec.instance[ClinicalTrialAgreement, ClinicalTrialAgreement, InvoiceError](
      (cta: ClinicalTrialAgreement) => cta,
      (cta: ClinicalTrialAgreement) => Right(cta.copy(note = "I is flipped!")))
}

object ApplicationTestsV5 extends App {

  import TestImplicits._

  val user1 = UserId("User1")
  val user2 = UserId("User2")

  val prodApp = new ProdApplication(new ProductionInvoiceRepo())
  val testApp = new TestApplication(new TestInvoiceRepo())
  // TODO [AD]: need to sort out the injection of the Codecs for the Repo layer (for transforming from persistence to
  //  domain and handling any business transformation processes), and those for the outer layer for transforming into a
  //  view to be consumed by a client of the application. Since they are both currently of type Codec[A,B] and are
  //  implicit parameters, when the types match the same Codec may be used which is never what is intended. Either make
  //  this non-implicit, or change the types of the type classes so that there can be no ambiguity.
  val testProcessorApp = new TestApplicationWithProcessor(new TestInvoiceRepo(), new TestCtaRepo()(versioned = TestImplicits.ctaVersioned, codec = TestImplicits.ctaRepoCodec))

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
}
