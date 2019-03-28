package sample1.application

import cats.effect.IO
import cats.syntax.either._
import cats.{Id, ~>}
import sample1.application.persistence.clinicaltrialagreement.CtaPersistenceRepo
import sample1.application.persistence.invoice.InvoicePersistenceRepo
import sample1.domain._
import sample1.domain.command.{CreateCtaCmd, CtaRetrieveCommand, EntityResult}
import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId}
import sample1.domain.entity.{EntityRepoCodec, Versioned}
import sample1.domain.errors.{CtaError, InvoiceError}
import sample1.domain.invoice._
import sample1.domain.invoice.commands.AddCost.AddCostCmd
import sample1.domain.invoice.commands.Approve.ApproveCmd
import sample1.domain.invoice.commands.CreateRfiInvoice.CreateRfiInvoiceCmd
import sample1.domain.invoice.commands.CreateSiteInvoice.CreateSiteInvoiceCmd
import sample1.domain.invoice.commands.ExampleDomainService.{ExampleDomainServiceCmd, ExampleDomainServiceImpl}
import sample1.domain.invoice.commands.{FindAll, FindAllEntities}
import sample1.domain.invoice.mixincommands.{ApproveCmdMixin, ApproveCmdV2Mixin}
import sample1.domain.permissions._
import sample1.domain.shared.DateTime
import sample1.domain.user.UserId
import sample1.infrastructure.{InMemoryPersistenceRepo, TestCtaRepo, TestInvoiceRepo}

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

  implicit val invoiceToViewDecoder: Decoder[InvoiceView, EntityResult[Invoice, InvoiceUserPermissions, InvoiceAction], InvoiceError] =
    (b: EntityResult[Invoice, InvoiceUserPermissions, InvoiceAction]) => InvoiceView.create(b.entity, b.permissions)

  /**
    * CTA codec used to encode and decode between domain and persistence types (both the same type in this example) This
    * is where a business process transformation may be injected (i.e. between the domain repo and persistence repo
    * layers). Hence the encode must always succeed, but the decode may fail.
    */
  implicit val ctaRepoCodec: EntityRepoCodec[ClinicalTrialAgreement, ClinicalTrialAgreement, CtaError] =
    EntityRepoCodec.instance[ClinicalTrialAgreement, ClinicalTrialAgreement, CtaError](
      (cta: ClinicalTrialAgreement) => cta,
      (cta: ClinicalTrialAgreement) => Right(cta.copy(note = "I've been flipped!")))

  val invoiceRepoCodecV1: EntityRepoCodec[Invoice, Invoice, InvoiceError] =
    EntityRepoCodec.instance[Invoice, Invoice, InvoiceError](
      (invoice: Invoice) => invoice,
      (invoice: Invoice) => invoice match {
        case i: SiteInvoice => Right(i)
        case i: SponsorInvoice => Right(i)
      })

  val invoicePersistenceRepo: InvoicePersistenceRepo[Id, InvoiceId, Invoice] =
    new InvoicePersistenceRepo[Id, InvoiceId, Invoice] with InMemoryPersistenceRepo[Id, InvoiceId, Invoice, InvoiceError] {
      override def notFoundErrorF: InvoiceId => InvoiceError = InvoiceError.InvoiceNotFound

      override def staleErrorF: InvoiceId => InvoiceError = InvoiceError.StaleInvoiceError

      override def find(): Id[Either[InvoiceError, List[Invoice]]] =
        currentStateList.asRight[InvoiceError]
    }

  val ctaPersistenceRepo: CtaPersistenceRepo[Id, ClinicalTrialAgreementId, ClinicalTrialAgreement] =
    new CtaPersistenceRepo[Id, ClinicalTrialAgreementId, ClinicalTrialAgreement] with InMemoryPersistenceRepo[Id, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaError] {
      override def notFoundErrorF: ClinicalTrialAgreementId => CtaError = CtaError.CtaNotFound

      override def staleErrorF: ClinicalTrialAgreementId => CtaError = CtaError.StaleCtaError

      override def find(): Id[Either[CtaError, Seq[ClinicalTrialAgreement]]] =
        currentStateList.asRight[CtaError]
    }

  implicit val invoiceToInvoiceView: Transform[EntityResult[Invoice, InvoiceUserPermissions, InvoiceAction], InvoiceView, InvoiceError] =
    (from: EntityResult[Invoice, InvoiceUserPermissions, InvoiceAction]) => InvoiceView.create(from.entity, from.permissions)
}

object ApplicationTestsV5 extends App {

  import TestImplicits._

  val approver = UserId("Approver1")
  val standardUser = UserId("StandardUser1")
  val approverWithLimit = UserId("ApproverWithLimit1")

  val standardUserInvoicePermissions = InvoiceUserPermissions(Set(
    InvoicePermissions.Read,
    InvoicePermissions.AddCost,
    InvoicePermissions.ReadSiteInvoice,
    InvoicePermissions.ReadSponsorInvoice,
    InvoicePermissions.Create))

  val standardUserCtaPermissions = CtaUserPermissions(Set(
    CtaPermissions.Read,
    CtaPermissions.Create))

  val approverInvoicePermissions = InvoiceUserPermissions(Set(
    InvoicePermissions.Read,
    InvoicePermissions.ReadSiteInvoice,
    InvoicePermissions.ReadSponsorInvoice,
    InvoicePermissions.AddCost,
    InvoicePermissions.Approve,
    InvoicePermissions.RunDomainServices,
    InvoicePermissions.Create))

  val approverCtaUserPermissions = CtaUserPermissions(Set(
    CtaPermissions.Read,
    CtaPermissions.Approve,
    CtaPermissions.Create))

  val approverWithLimitInvoicePermissions = InvoiceUserPermissions(Set(
    InvoicePermissions.Read,
    InvoicePermissions.ReadSiteInvoice,
    InvoicePermissions.ReadSponsorInvoice,
    InvoicePermissions.AddCost,
    InvoicePermissions.Approve,
    InvoicePermissions.RunDomainServices,
    InvoicePermissions.ApproveWithLimit(10000),
    InvoicePermissions.Create))

  val approverWithLimitCtaUserPermissions = CtaUserPermissions(Set(
    CtaPermissions.Read,
    CtaPermissions.Approve,
    CtaPermissions.Create))

  val cost1usd: Cost = Cost(MonetaryAmount(1200, Currency("USD")))
  val cost2usd: Cost = Cost(MonetaryAmount(5005, Currency("USD")))
  val cost3usd: Cost = Cost(MonetaryAmount(7005, Currency("USD")))
  val cost1gbp: Cost = Cost(MonetaryAmount(300, Currency("GBP")))

  // TODO [AD]: consider naming of the application transformer and the repo codec and also their current inheritance
  //  structure (i.e. deriving from the Codec family of traits). Also, should these simply be explicit parameters?
  val testProcessorApp = new TestApplication(
    new TestInvoiceRepo(invoicePersistenceRepo, invoiceRepoCodecV1),
    new TestCtaRepo(ctaPersistenceRepo, ctaRepoCodec),
    ExampleDomainServiceImpl())

  case class TestInvoiceBasicPermissionRetriever() extends InvoiceBasicPermissionRetriever[Id] {
    override def retrieve(userId: UserId): Id[InvoiceUserPermissions] = userId match {
      case UserId(ApplicationTestsV5.standardUser.id) => standardUserInvoicePermissions
      case UserId(ApplicationTestsV5.approver.id) => approverInvoicePermissions
      case UserId(ApplicationTestsV5.approverWithLimit.id) => approverWithLimitInvoicePermissions
      case _ => InvoiceUserPermissions(Set())
    }
  }

  case class TestInvoiceEntityPermissionRetriever() extends InvoiceEntityPermissionRetriever[Id] {
    override def retrieve(userId: UserId, entity: Invoice): Id[InvoiceUserPermissions] = userId match {
      case UserId(ApplicationTestsV5.standardUser.id) => standardUserInvoicePermissions
      case UserId(ApplicationTestsV5.approver.id) => approverInvoicePermissions
      case UserId(ApplicationTestsV5.approverWithLimit.id) => approverWithLimitInvoicePermissions
      case _ => InvoiceUserPermissions(Set())
    }
  }

  case class TestCtaBasicPermissionRetriever() extends CtaBasicPermissionRetriever[Id] {
    override def retrieve(userId: UserId): Id[CtaUserPermissions] = userId match {
      case UserId(ApplicationTestsV5.standardUser.id) => standardUserCtaPermissions
      case UserId(ApplicationTestsV5.approver.id) => approverCtaUserPermissions
      case UserId(ApplicationTestsV5.approverWithLimit.id) => approverWithLimitCtaUserPermissions
      case _ => CtaUserPermissions(Set())
    }
  }

  case class TestCtaEntityPermissionRetriever() extends CtaEntityPermissionRetriever[Id] {
    override def retrieve(userId: UserId, entity: ClinicalTrialAgreement): Id[CtaUserPermissions] = userId match {
      case UserId(ApplicationTestsV5.standardUser.id) => standardUserCtaPermissions
      case UserId(ApplicationTestsV5.approver.id) => approverCtaUserPermissions
      case UserId(ApplicationTestsV5.approverWithLimit.id) => approverWithLimitCtaUserPermissions
      case _ => CtaUserPermissions(Set())
    }
  }

  val testBasicPermissionsRetriever = TestInvoiceBasicPermissionRetriever()
  val testEntityPermissionsRetriever = TestInvoiceEntityPermissionRetriever()

  val testCtaBasicPermissionsRetriever = TestCtaBasicPermissionRetriever()
  val testCtaEntityPermissionsRetriever = TestCtaEntityPermissionRetriever()

  val res2 = for {
    inv1 <- testProcessorApp.processCommand(ExampleDomainServiceCmd(approver, testBasicPermissionsRetriever))
  } yield inv1
  println(s"res2: $res2")

  val res3 = for {
    inv1 <- testProcessorApp.processCommand(ExampleDomainServiceCmd(standardUser, testBasicPermissionsRetriever))
  } yield inv1
  println(s"res3: $res3")

  val res4 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    _ <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(AddCostCmd(approver, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever, cost1usd))
    _ <- testProcessorApp.processCommand(AddCostCmd(approver, inv2.entity.id, inv2.entity.version, testEntityPermissionsRetriever, cost2usd))
    res <- testProcessorApp.processCommand(FindAll(approver, testBasicPermissionsRetriever))
  } yield res
  println(s"res4: $res4")

  val res5 = for {
    inv1 <- testProcessorApp.processCommand(FindAllEntities(approver, testBasicPermissionsRetriever))
  } yield inv1
  println(s"res5: $res5")

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
    inv2 <- testProcessorApp.processCommand(ApproveCmd(approver, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2
  println(s"res9: $res9")

  val res10 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmd(approverWithLimit, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2
  println(s"res10: $res10")

  //  val res11 = for {
  //    inv: EntityResult[Invoice, InvoiceUserPermissions, InvoiceAction] <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(standardUser, testBasicPermissionsRetriever))
  //    invRetrieved <- testProcessorApp.processCommand(InvoiceRetrieveCommand(standardUser, inv.entity.id, testEntityPermissionsRetriever)).to[InvoiceView]
  //  } yield invRetrieved
  //  println(s"res11: $res11")

  val res12 = for {
    cta <- testProcessorApp.processCommand(CreateCtaCmd(standardUser, testCtaBasicPermissionsRetriever, DateTime.now()))
  } yield cta
  println(s"res12: $res12")

  val res13 = for {
    cta <- testProcessorApp.processCommand(CreateCtaCmd(standardUser, testCtaBasicPermissionsRetriever, DateTime.now()))
    ctaRetrieved <- testProcessorApp.processCommand(CtaRetrieveCommand(standardUser, cta.entity.id, testCtaEntityPermissionsRetriever))
  } yield ctaRetrieved
  println(s"res13: $res13")


  val res15 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(ApproveCmd(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever))
  } yield inv2
  println(s"res15: $res15")

  val res16 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(AddCostCmd(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever, cost1usd))
    inv3 <- testProcessorApp.processCommand(ApproveCmd(approverWithLimit, inv2.entity.id, inv2.entity.version, testEntityPermissionsRetriever))
  } yield inv3
  println(s"res16: $res16")

  val res17 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(AddCostCmd(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever, cost1usd))
    inv3 <- testProcessorApp.processCommand(AddCostCmd(standardUser, inv2.entity.id, inv2.entity.version, testEntityPermissionsRetriever, cost2usd))
    inv4 <- testProcessorApp.processCommand(AddCostCmd(standardUser, inv3.entity.id, inv3.entity.version, testEntityPermissionsRetriever, cost3usd))
    inv5 <- testProcessorApp.processCommand(ApproveCmd(approverWithLimit, inv4.entity.id, inv4.entity.version, testEntityPermissionsRetriever))
  } yield inv5
  println(s"res17: $res17")

  val res18 = for {
    inv1 <- testProcessorApp.processCommand(CreateRfiInvoiceCmd(approver, testBasicPermissionsRetriever))
    inv2 <- testProcessorApp.processCommand(AddCostCmd(standardUser, inv1.entity.id, inv1.entity.version, testEntityPermissionsRetriever, cost1usd))
    inv3 <- testProcessorApp.processCommand(AddCostCmd(standardUser, inv2.entity.id, inv2.entity.version, testEntityPermissionsRetriever, cost1gbp))
    inv4 <- testProcessorApp.processCommand(ApproveCmd(approverWithLimit, inv3.entity.id, inv3.entity.version, testEntityPermissionsRetriever))
  } yield inv4
  println(s"res18: $res18")

  //  val res17 = for {
  //    res <- testProcessorApp.processCommand(FindAll(standardUser, testBasicPermissionsRetriever)).to[Seq[InvoiceView]]
  //  } yield res
  //  println(s"res17: $res17")

  val transformer = (res: EntityResult[Invoice, InvoiceUserPermissions, InvoiceAction]) => InvoiceView.create(res.entity, res.permissions)

  val res19 = for {
    inv1 <- testProcessorApp.processCommand(CreateSiteInvoiceCmd(standardUser, testBasicPermissionsRetriever), transformer)
    inv2 <- testProcessorApp.processCommand(ApproveCmd(approver, inv1.id, inv1.version, testEntityPermissionsRetriever), transformer)
  } yield inv2
  println(s"res19: $res19")

}
