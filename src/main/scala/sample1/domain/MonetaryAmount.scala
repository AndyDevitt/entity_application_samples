package sample1.domain

import cats.data.Validated

final case class Currency private(code: String) extends AnyVal

object Currency {
  def create(code: String): Validated[ValidationError, Currency] =
    Validated.cond(code.length == 3, Currency(code), InvalidCurrencyCode(code))
}

final case class MonetaryAmount private(amount: BigDecimal, currency: Currency)

object MonetaryAmount {
  def create(amount: BigDecimal, code: String): Validated[ValidationError, MonetaryAmount] =
    Currency.create(code).map(MonetaryAmount(amount, _))

  def create(amount: BigDecimal, code: Currency): MonetaryAmount =
    MonetaryAmount(amount, code)
}

object MonetaryAmountAlg {
  def sum(first: MonetaryAmount, second: MonetaryAmount): Either[ValidationError, MonetaryAmount] =
    Either.cond[ValidationError, MonetaryAmount](first.currency == second.currency,
      MonetaryAmount.create(first.amount + second.amount, first.currency),
      CurrencyMismatch(first.currency, second.currency))
}
