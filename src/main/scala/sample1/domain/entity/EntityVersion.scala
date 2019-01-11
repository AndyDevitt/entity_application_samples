package sample1.domain.entity

final case class EntityVersion(version: Int) {
  def nextVersion: EntityVersion = copy(version = version + 1)

  def >(other: EntityVersion): Boolean = this.version > other.version

  def <(other: EntityVersion): Boolean = this.version < other.version
}

object EntityVersion {
  def apply(): EntityVersion = new EntityVersion(0)
}
