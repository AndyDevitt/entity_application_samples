package sample1.domain.invoice

import cats.Id
import sample1.domain.command.{Command, CreateRfiInvoiceCmd, CreateRfiInvoiceCmdG, CreateSiteInvoiceCmdG}
import sample1.domain.{RequestForInvoice, UserId}

object InvoiceStateBuilder {

  private[InvoiceStateBuilder] sealed trait Buildable

  final case object CanBuild extends Buildable

  final case object CannotBuild extends Buildable

  /**
    * Builder class to update Invoice state. Provides an interface describing all the available state manipulation
    * functions, but delegates implementation to implicitly defined instances of the type class specific to each
    * behaviour. This enables behaviour to be defined only for the types in the ADT hierarchy where the behaviour is
    * valid, and invalid calls to update state will be detected at compile time.
    *
    * @param invoice the invoice state that is being manipulated
    * @tparam A the specific type of the invoice
    */
  implicit class BuilderOps[A <: Invoice](override protected val invoice: A) extends Builder[A, CannotBuild.type]

  trait Builder[A <: Invoice, B <: Buildable] {
    protected def invoice: A

    /**
      * build returns the underlying type A, whereas every other method returns a Builder[A]. This ensures that build is
      * called to complete the build and guarantees that lastEditedBy and lastEditedAt (and any other mandatory field to
      * update) is handled by updating them in this method.
      *
      * @param cmd  the command that is being executed to update the state
      * @param impl implementation for the build behaviour provided via implicit resolution
      * @return returns the underlying type from the builder object
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

  /**
    * Implicit instances to implement each behaviour trait for valid ADT types. Super-type instances delegate to
    * sub-type instances to guarantee consistent implementation.
    */
  object Instances {

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

  }

}

object InvoiceStateBuilderTest {

  import InvoiceStateBuilder.Instances._
  import InvoiceStateBuilder._

  val siteInv: SiteInvoice = Invoice.createSiteInvoice(CreateSiteInvoiceCmdG(UserId()))
  val sponsorInv: SponsorInvoice = Invoice.createRfiInvoice(CreateRfiInvoiceCmd(UserId()))
  val inv: Invoice = sponsorInv
  val cmd: CreateRfiInvoiceCmdG[Id] = CreateRfiInvoiceCmdG[Id](UserId())

  val updated: SponsorInvoice = sponsorInv
    .clearCosts()
    .updateRfi(RequestForInvoice())
    .updateLastEdited(cmd)
    .build()

  val updatedSite: SiteInvoice = siteInv
    .clearCosts()
    // The following line will not compile since there is no implementation for UpdateRfi[SiteInvoice]
    // .updateRfi(RequestForInvoice())
    .updateLastEdited(cmd)
    .build()

  val updatedInv: Invoice = inv
    .updateLastEdited(cmd)
    .clearCosts()
    // The following line will not compile since there is no implementation for UpdateRfi[Invoice]
    // .updateRfi(RequestForInvoice())
    .build()

}
