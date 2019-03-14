package sample1.domain.permissions

trait EntityPermissions[PermissionsType] {
  def permissions: Set[PermissionsType]

  def hasPermission(permission: PermissionsType): Boolean = permissions.contains(permission)

  def hasAll(permissionsIn: Set[PermissionsType]): Boolean = permissionsIn.forall(permissions contains _)

  def hasAny(permissionsIn: Set[PermissionsType]): Boolean = permissionsIn.exists(permissions contains _)
}
