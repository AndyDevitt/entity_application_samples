package sample1.domain.user

final case class UserWithPermissions(userId: UserId, permissions: Set[UserWithPermissions])
