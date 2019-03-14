package sample1.application

import cats.effect.IO
import cats.{Id, ~>}
import sample1.domain._
import sample1.domain.command._
import sample1.domain.cta.ClinicalTrialAgreement
import sample1.domain.entity.{EntityRepoCodec, Versioned}
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.{Invoice, SiteInvoice, SponsorInvoice}
import sample1.domain.permissions.{InvoiceBasicPermissionRetriever, InvoiceEntityPermissionRetriever, InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.user.UserId
import sample1.infrastructure.{TestCtaRepo, TestInvoiceRepo}

import scala.concurrent.Future
import scala.language.postfixOps

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

  //  implicit val invoiceToViewDecoder: Decoder[InvoiceView, Invoice, InvoiceError] =
  //    (b: Invoice) => InvoiceView.create(b)

  /**
    * CTA codec used to encode and decode between domain and persistence types (both the same type in this example) This
    * is where a business process transformation may be injected (i.e. between the domain repo and persistence repo
    * layers). Hence the encode must always succeed, but the decode may fail.
    */
  implicit val ctaRepoCodec: EntityRepoCodec[ClinicalTrialAgreement, ClinicalTrialAgreement, InvoiceError] =
    EntityRepoCodec.instance[ClinicalTrialAgreement, ClinicalTrialAgreement, InvoiceError](
      (cta: ClinicalTrialAgreement) => cta,
      (cta: ClinicalTrialAgreement) => Right(cta.copy(note = "I've been flipped!")))

  //    implicit val invoiceToInvoiceView: Transform[Invoice, InvoiceView, InvoiceError] =
  //      (from: Invoice) => InvoiceView.create(from)
}

object ApplicationTestsV5 extends App {

  import TestImplicits._

  val user1 = UserId("User1")
  val user2 = UserId("User2")

  // TODO [AD]: consider naming of the application transformer and the repo codec and also their current inheritance
  //  structure (i.e. deriving from the Codec family of traits). Also, should these simply be explicit parameters?
  val testProcessorApp = new TestApplication(new TestInvoiceRepo(), new TestCtaRepo())

  case class TestInvoiceBasicPermissionRetriever() extends InvoiceBasicPermissionRetriever[Id] {
    override def retrieve(userId: UserId): Id[InvoiceUserPermissions] =
      InvoiceUserPermissions(Set(InvoicePermissions.Create()))
  }

  case class TestInvoiceEntityPermissionRetriever() extends InvoiceEntityPermissionRetriever[Id] {
    override def retrieve(userId: UserId, entity: Invoice): Id[InvoiceUserPermissions] =
      InvoiceUserPermissions(Set(InvoicePermissions.Read()))
  }

  val testEntityPermissionsRetriever = TestInvoiceEntityPermissionRetriever()
  val testBasicPermissionsRetriever = TestInvoiceBasicPermissionRetriever()

  val res8 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(user1, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmd(user2, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2

  println(s"res8: $res8")

  val res9 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(user1, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmd(user2, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2

  println(s"res9: $res9")

  val res10 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(user1, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmd(user2, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2

  println(s"res10: $res10")

  //  val res11 = for {
  //    inv <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(user1))
  //    invRetrieved <- testProcessorApp.processCommand(InvoiceRetrieveCommand(user1, inv.id)).to[InvoiceView]
  //  } yield invRetrieved
  //
  //  println(s"res11: $res11")

  //  val res12 = for {
  //    cta <- testProcessorApp.processCommand(CreateCtaCmd(user1))
  //  } yield cta
  //
  //  println(s"res12: $res12")
  //
  //  val res13 = for {
  //    cta <- testProcessorApp.processCommand(CreateCtaCmd(user1))
  //    ctaRetrieved <- testProcessorApp.processCommand(CtaRetrieveCommand(user1, cta.id))
  //  } yield ctaRetrieved
  //
  //  println(s"res13: $res13")


  val res15 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(user1, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmdV2(user2, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2

  println(s"res15: $res15")

  val res16 = for {
    inv1 <- testProcessorApp.processCommand(CreateSiteInvoiceCmd(user1, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmdV2(user2, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2

  println(s"res16: $res16")

  //  val res17 = for {
  //    res <- testProcessorApp.processCommand(FindAll(user1)).to[Seq[InvoiceView]]
  //  } yield res
  //
  //  println(s"res17: $res17")

  //  val transformer = (inv: Invoice) => InvoiceView.create(inv)
  //
  //  val res18 = for {
  //    inv1 <- testProcessorApp.processCommand(CreateSiteInvoiceCmd(user1), transformer)
  //    inv2 <- testProcessorApp.processCommand(ApproveCmdV2(user2, inv1.id, inv1.version), transformer)
  //  } yield inv2
  //
  //  println(s"res18: $res18")

}
