package sample1.domain.invoice

import cats.Id
import sample1.application.ApplicationTestsV5.{TestInvoiceBasicPermissionRetriever, TestInvoiceEntityPermissionRetriever}
import sample1.domain.command.Command
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.commands.CreateRfiInvoice.CreateRfiInvoiceCmd
import sample1.domain.user.UserId
import sample1.domain.{Cost, Currency, MonetaryAmount}

object InvoiceStateBuilder {

  private[InvoiceStateBuilder] sealed trait Buildable

  final case object CanBuild extends Buildable

  final case object CannotBuild extends Buildable

  /**
    * BuilderOps class to provide an implicit conversion on any Invoice instance for ease of use.
    *
    * @param invoice the invoice state that is being manipulated
    * @tparam A the specific type of the invoice
    */
  implicit class BuilderOps[A <: Invoice](override protected val invoice: A) extends Builder[A, CannotBuild.type]

  /**
    * Builder trait to update Invoice state. Provides an interface describing all the available state manipulation
    * functions, but delegates implementation to implicitly defined instances of the type class specific to each
    * behaviour. This enables behaviour to be defined only for the types in the ADT hierarchy where the behaviour is
    * valid, and invalid calls to update state will be detected at compile time.
    *
    * Follows the builder pattern using Phantom types to ensure that the updateLastEdited() method is called before the
    * underlying invoice state can be obtained, thus guaranteeing that certain required updates are performed for every
    * state update. If other methods are required to be called, additional flags may be added to enforce the same rules.
    *
    * @tparam A
    * @tparam B
    */
  trait Builder[A <: Invoice, B <: Buildable] {
    protected def invoice: A

    /**
      * build method returns the underlying state after any mutations have taken place. It requires that the generic
      * type B is of type CanBuild in order to compile when called, ensuring that the underlying state cannot be
      * retrieved until this type has been encoded on the Builder instance.
      *
      * @param ev implicit evidence that the type B is indeed CanBuild.type
      * @return the underlying invoice
      */
    def build()(implicit ev: B =:= CanBuild.type): A =
      invoice

    def updateLastEdited(cmd: Command)(implicit impl: UpdateLastEdited[A]): Builder[A, CanBuild.type] =
      Builder(impl.updateLastEdited(invoice, cmd))

    def clearCosts()(implicit impl: ClearCosts[A]): Builder[A, B] =
      Builder(impl.clearCosts(invoice))

    def setStatus(status: InvoiceStatus)(implicit impl: SetStatus[A]): Builder[A, B] =
      Builder(impl.setStatus(invoice, status))

    def updateRfi(requestForInvoice: RequestForInvoice)(implicit impl: UpdateRfi[A]): Builder[A, B] =
      Builder(impl.updateRfi(invoice, requestForInvoice))

    def addCost(cost: Cost)(implicit impl: AddCost[A]): Either[InvoiceError, Builder[A, B]] =
      impl.addCost(invoice, cost).map(Builder(_))
  }

  object Builder {
    def apply[A <: Invoice, B <: Buildable](inv: A): Builder[A, B] = new Builder[A, B] {
      override def invoice: A = inv
    }
  }

  /**
    * Individual traits to define each state update behaviour
    */

  trait ClearCosts[A] {
    def clearCosts(a: A): A
  }

  trait UpdateLastEdited[A] {
    def updateLastEdited(a: A, cmd: Command): A
  }

  trait SetStatus[A] {
    def setStatus(a: A, status: InvoiceStatus): A
  }

  trait UpdateRfi[A] {
    def updateRfi(a: A, rfi: RequestForInvoice): A
  }

  trait AddCost[A] {
    def addCost(a: A, cost: Cost): Either[InvoiceError, A]
  }

  /**
    * Implicit instances to implement each behaviour trait for valid ADT types. Super-type instances delegate to
    * sub-type instances to guarantee consistent implementation.
    */

  implicit val updateLastEditedSite: UpdateLastEdited[SiteInvoice] = (inv, cmd) => inv.copy(lastEditedBy = cmd.userId)
  implicit val updateLastEditedSponsor: UpdateLastEdited[SponsorInvoice] = (inv, cmd) => inv.copy(lastEditedBy = cmd.userId)

  implicit def updateLastEditedInvoice(implicit siteImpl: UpdateLastEdited[SiteInvoice], sponsorImpl: UpdateLastEdited[SponsorInvoice]): UpdateLastEdited[Invoice] = (inv, cmd) => inv match {
    case i: SiteInvoice => siteImpl.updateLastEdited(i, cmd)
    case i: SponsorInvoice => sponsorImpl.updateLastEdited(i, cmd)
  }

  implicit val clearCostsSite: ClearCosts[SiteInvoice] = inv => inv.copy(costs = Nil)
  implicit val clearCostsSponsor: ClearCosts[SponsorInvoice] = inv => inv.copy(costs = Nil)

  implicit def clearCostsInvoice(implicit siteImpl: ClearCosts[SiteInvoice], sponsorImpl: ClearCosts[SponsorInvoice]): ClearCosts[Invoice] = {
    case i: SiteInvoice => siteImpl.clearCosts(i)
    case i: SponsorInvoice => sponsorImpl.clearCosts(i)
  }

  implicit val setStatusSite: SetStatus[SiteInvoice] = (inv, status) => inv.copy(status = status)
  implicit val setStatusSponsor: SetStatus[SponsorInvoice] = (inv, status) => inv.copy(status = status)

  implicit def setStatusInvoice(implicit siteImpl: SetStatus[SiteInvoice], sponsorImpl: SetStatus[SponsorInvoice]): SetStatus[Invoice] = (inv, status) => inv match {
    case i: SiteInvoice => siteImpl.setStatus(i, status)
    case i: SponsorInvoice => sponsorImpl.setStatus(i, status)
  }

  implicit val updateRfiSponsor: UpdateRfi[SponsorInvoice] = (inv, rfi) => inv.copy(rfi = rfi)

  implicit val addCostSite: AddCost[SiteInvoice] = (inv, cost) => Right(inv.copy(costs = inv.costs :+ cost))
  implicit val addCostSponsor: AddCost[SponsorInvoice] = (inv, cost) => Right(inv.copy(costs = inv.costs :+ cost))

  implicit def addCostInvoice(implicit siteImpl: AddCost[SiteInvoice], sponsorImpl: AddCost[SponsorInvoice]): AddCost[Invoice] = (inv, cost) => inv match {
    case i: SiteInvoice => siteImpl.addCost(i, cost)
    case i: SponsorInvoice => sponsorImpl.addCost(i, cost)
  }

}

object InvoiceStateBuilderTest {

  import InvoiceStateBuilder._

  val testEntityPermissionsRetriever = TestInvoiceEntityPermissionRetriever()
  val testBasicPermissionsRetriever = TestInvoiceBasicPermissionRetriever()

  val siteInv: SiteInvoice = Invoice.createSiteInvoiceEmpty[Id]()
  val sponsorInv: SponsorInvoice = Invoice.createRfiInvoiceEmpty[Id]()
  val inv: Invoice = sponsorInv
  val cmd: CreateRfiInvoiceCmd[Id] = CreateRfiInvoiceCmd[Id](UserId(), testBasicPermissionsRetriever)
  val cost: Cost = Cost(MonetaryAmount(0, Currency("USD")))

  val updated: SponsorInvoice = sponsorInv
    .clearCosts()
    .updateRfi(RequestForInvoice())
    .updateLastEdited(cmd)
    .build()

  val updatedSite: SiteInvoice = siteInv
    .clearCosts()
    // The following line will not compile since there is no implementation for UpdateRfi[SiteInvoice]
    //.updateRfi(RequestForInvoice())
    .updateLastEdited(cmd)
    .build()

  val updatedInv: Invoice = inv
    .updateLastEdited(cmd)
    .clearCosts()
    // The following line will not compile since there is no implementation for UpdateRfi[Invoice]
    // .updateRfi(RequestForInvoice())
    .build()

  val updateWithFailure: Either[InvoiceError, SponsorInvoice] = sponsorInv
    .addCost(cost)
    .map(_.clearCosts())
    .map(_.updateRfi(RequestForInvoice()))
    .map(_.updateLastEdited(cmd))
    .flatMap(_.addCost(cost))
    .map(_.build())

}
