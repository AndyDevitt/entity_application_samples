package sample1.application

import cats.effect.IO
import cats.{Id, ~>}
import sample1.domain.command.invoicecommands._
import sample1.domain.cta.ClinicalTrialAgreement
import sample1.domain.entity.{EntityRepoCodec, Versioned}
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.{Invoice, SiteInvoice, SponsorInvoice}
import sample1.domain.permissions.{InvoiceBasicPermissionRetriever, InvoiceEntityPermissionRetriever, InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.user.UserId
import sample1.domain.{Cost, Currency, MonetaryAmount}
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

  val approver = UserId("Approver1")
  val standardUser = UserId("StandardUser1")
  val approverWithLimit = UserId("ApproverWithLimit1")

  val standardUserPermissions = InvoiceUserPermissions(Set(
    InvoicePermissions.Read,
    InvoicePermissions.AddCost,
    InvoicePermissions.ReadSiteInvoice,
    InvoicePermissions.ReadSponsorInvoice,
    InvoicePermissions.Create))

  val approverPermissions = InvoiceUserPermissions(Set(
    InvoicePermissions.Read,
    InvoicePermissions.ReadSiteInvoice,
    InvoicePermissions.ReadSponsorInvoice,
    InvoicePermissions.AddCost,
    InvoicePermissions.Approve,
    InvoicePermissions.Create))

  val approverWithLimitPermissions = InvoiceUserPermissions(Set(
    InvoicePermissions.Read,
    InvoicePermissions.ReadSiteInvoice,
    InvoicePermissions.ReadSponsorInvoice,
    InvoicePermissions.AddCost,
    InvoicePermissions.Approve,
    InvoicePermissions.ApproveWithLimit(10000),
    InvoicePermissions.Create))

  val cost1usd: Cost = Cost(MonetaryAmount(1200, Currency("USD")))
  val cost2usd: Cost = Cost(MonetaryAmount(5005, Currency("USD")))
  val cost3usd: Cost = Cost(MonetaryAmount(7005, Currency("USD")))
  val cost1gbp: Cost = Cost(MonetaryAmount(300, Currency("GBP")))

  // TODO [AD]: consider naming of the application transformer and the repo codec and also their current inheritance
  //  structure (i.e. deriving from the Codec family of traits). Also, should these simply be explicit parameters?
  val testProcessorApp = new TestApplication(new TestInvoiceRepo(), new TestCtaRepo())

  case class TestInvoiceBasicPermissionRetriever() extends InvoiceBasicPermissionRetriever[Id] {
    override def retrieve(userId: UserId): Id[InvoiceUserPermissions] = userId match {
      case UserId(ApplicationTestsV5.standardUser.id) => standardUserPermissions
      case UserId(ApplicationTestsV5.approver.id) => approverPermissions
      case UserId(ApplicationTestsV5.approverWithLimit.id) => approverWithLimitPermissions
      case _ => InvoiceUserPermissions(Set())
    }
  }

  case class TestInvoiceEntityPermissionRetriever() extends InvoiceEntityPermissionRetriever[Id] {
    override def retrieve(userId: UserId, entity: Invoice): Id[InvoiceUserPermissions] = userId match {
      case UserId(ApplicationTestsV5.standardUser.id) => standardUserPermissions
      case UserId(ApplicationTestsV5.approver.id) => approverPermissions
      case UserId(ApplicationTestsV5.approverWithLimit.id) => approverWithLimitPermissions
      case _ => InvoiceUserPermissions(Set())
    }
  }

  val testEntityPermissionsRetriever = TestInvoiceEntityPermissionRetriever()
  val testBasicPermissionsRetriever = TestInvoiceBasicPermissionRetriever()


  val res6 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmdV2Mixin(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2

  println(s"res6: $res6")

  val res7 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmdMixin(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2

  println(s"res7: $res7")

  val res8 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmd(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2

  println(s"res8: $res8")

  val res9 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmd(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2

  println(s"res9: $res9")

  val res10 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmd(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
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
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmdV2(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2

  println(s"res15: $res15")

  val res16 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(AddCostCmd(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever, cost1usd))
    inv3 <- testProcessorApp.processCommand(ApproveCmdV2(approverWithLimit, inv2.entity.id, inv2.entity.version, testEntityPermissionsRetriever))
  } yield inv3

  println(s"res16: $res16")

  val res17 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(AddCostCmd(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever, cost1usd))
    inv3 <- testProcessorApp.processCommand(AddCostCmd(standardUser, inv2.entity.id, inv2.entity.version, testEntityPermissionsRetriever, cost2usd))
    inv4 <- testProcessorApp.processCommand(AddCostCmd(standardUser, inv3.entity.id, inv3.entity.version, testEntityPermissionsRetriever, cost3usd))
    inv5 <- testProcessorApp.processCommand(ApproveCmdV2(approverWithLimit, inv4.entity.id, inv4.entity.version, testEntityPermissionsRetriever))
  } yield inv5

  println(s"res17: $res17")

  val res18 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(AddCostCmd(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever, cost1usd))
    inv3 <- testProcessorApp.processCommand(AddCostCmd(standardUser, inv2.entity.id, inv2.entity.version, testEntityPermissionsRetriever, cost1gbp))
    inv4 <- testProcessorApp.processCommand(ApproveCmdV2(approverWithLimit, inv3.entity.id, inv3.entity.version, testEntityPermissionsRetriever))
  } yield inv4

  println(s"res18: $res18")

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
