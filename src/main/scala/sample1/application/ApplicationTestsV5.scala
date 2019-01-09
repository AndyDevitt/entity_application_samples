package sample1.application

import cats.data.EitherT
import cats.effect.IO
import cats.instances.future._
import cats.{Id, ~>}
import sample1.domain._
import sample1.domain.command.{ApproveCmd, ApproveCmdG, CreateRfiInvoiceCmd, UpdateRfiCmd}
import sample1.domain.entity.{EntityVersion, Versioned}
import sample1.domain.invoice.{Invoice, SiteInvoice, SponsorInvoice}
import sample1.infrastructure.{ProductionRepo, TestRepo}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.{higherKinds, postfixOps}
import scala.util.{Failure, Success}

object TestImplicits {

  implicit val IoToFuture: IO ~> Future = new ~>[IO, Future] {
    override def apply[A](fa: IO[A]): Future[A] = fa.unsafeToFuture()
  }

  implicit val IdToId: Id ~> Id = new ~>[Id, Id] {
    override def apply[A](fa: Id[A]): Id[A] = fa
  }

  implicit val invoiceVersioned: Versioned[Invoice] = Versioned.instance {
    case i: SiteInvoice => i.copy(version = i.version.nextVersion)
    case i: SponsorInvoice => i.copy(version = i.version.nextVersion)
  }

  implicit val invoiceToViewDecoder: Decoder[InvoiceView, Invoice, InvoiceError] =
    (b: Invoice) => InvoiceView.create(b)

}

object ApplicationTestsV5 extends App {

  import TestImplicits._

  val user1 = UserId("User1")
  val user2 = UserId("User2")

  val prodApp = new ProdApplication(new ProductionRepo())
  val testApp = new TestApplication(new TestRepo())

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
    inv1 <- testApp.createRfiInvoice(CreateRfiInvoiceCmd(user1))
    inv2 <- testApp.processCommand(ApproveCmdG[Id](user2, inv1.id, inv1.version))
  } yield inv2

  println(s"res8: $res8")
}
