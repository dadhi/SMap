package example.map

import scala.reflect.ClassTag

/** The type of value `V` is not covariant as in the `Map[K, +V]` because SMap
  * allows to modify the value inside the entry. So in a sense it is
  * semi-immutable data structure which may be used mostly as immutable and
  * additionally to have holes for values to be set or updated later. The value
  * location is fixed in memory so you may grab a reference to the SMap.Entry
  * and update it later without worries of SMap stucture additions or
  * modifications.
  */
trait SMap[K, V] {
  import SMap._

  /** Indicates that the map is empty
    */
  def isEmpty: Boolean = this eq Empty

  val count: Int = 0

  protected def getMinHashEntryOrNull: Entry[K, V] = null
  protected def getMaxHashEntryOrNull: Entry[K, V] = null

  /** Lookup for the entry by hash. If nothing the method returns `null`
    */
  protected def getEntryOrNull(hash: Int): Entry[K, V] = null

  /** Returns the found entry with the same hash or the new map with added new
    * entry. Note that the empty map will return the entry the same as if the
    * entry was found - so the consumer should check for the empty map. Note
    * that the method cannot return the `null` - when the existing entry is not
    * found it will alway be the new map with the added entry.
    */
  def addOrGetEntry(hash: Int, entry: Entry[K, V]): SMap[K, V] = entry

  /** Returns the new map with old entry replaced by the new entry. Note that
    * the old entry should be present
    */
  def replaceEntry(
      hash: Int,
      oldEntry: Entry[K, V],
      newEntry: Entry[K, V]
  ): SMap[K, V] = this

  /** Removes the certainly present old entry and returns the new map without
    * the entry.
    */
  protected def removeEntry(entry: Entry[K, V]): SMap[K, V] = this

  /** The function is supposed to return the entry different from the oldEntry
    * to update, and return the oldEntry to keep it.
    */
  type UpdaterOrKeeper[S] =
    ((S, KVEntry[K, V], KVEntry[K, V]) => KVEntry[K, V]) {
      def apply(
          state: S,
          oldEntry: KVEntry[K, V],
          newEntry: KVEntry[K, V]
      ): KVEntry[K, V]
    }

  def get[K, V](key: K): Option[V] = {
    var e = getEntryOrNull(key.hashCode)
    e match {
      case KVEntry(_, k, v) =>
        if (k == key) Some(v.asInstanceOf[V])
        else None // todo: @wip can we avoid the cast here?
      case HashConflictingEntry(_, conflicts) =>
        conflicts
          .find(key.==)
          .map(
            _.value.asInstanceOf[V]
          ) // todo: @wip can we avoid the cast here?
      case _ => None
    }
  }
}

object SMap {

  private case object Empty extends SMap[Any, Nothing]

  def empty[K, V]: SMap[K, V] = Empty.asInstanceOf[SMap[K, V]]

  def newEntry[K, V](item: (K, V)): KVEntry[K, V] =
    KVEntry(item._1.hashCode, item._1, item._2)

  def apply[K, V](item: (K, V)): SMap[K, V] = newEntry(item)

  def apply[K, V](item1: (K, V), item2: (K, V)): SMap[K, V] =
    Leaf2(newEntry(item1), newEntry(item2))

  def apply[K, V](items: (K, V)*): SMap[K, V] = {
    val m = empty[K, V]
    for (i <- items) m.addOrGetEntry(i._1.hashCode, newEntry(i))
    m
  }

  abstract class Entry[K, V](hash: Int) extends SMap[K, V] {

    override def getMinHashEntryOrNull: Entry[K, V] = this
    override def getMaxHashEntryOrNull: Entry[K, V] = this
    override def getEntryOrNull(hash: Int): Entry[K, V] =
      if (hash == this.hash) this else null

    /** Lookup for the entry by Hash and Key
      */
    def getEntryOrNull(hash: Int, key: K): KVEntry[K, V]

    /** Updating the entry with the new one
      */
    def update(newEntry: KVEntry[K, V]): Entry[K, V]

    /** Updating the entry with the new one using the `update` method
      */
    def updateOrKeep[S](
        state: S,
        newEntry: KVEntry[K, V],
        updateOrKeep: UpdaterOrKeeper[S]
    ): Entry[K, V]

    override def addOrGetEntry(hash: Int, entry: Entry[K, V]): SMap[K, V] =
      if (hash > this.hash) new Leaf2(this, entry)
      else if (hash < this.hash) new Leaf2(entry, this)
      else this

    override def replaceEntry(
        hash: Int,
        oldEntry: Entry[K, V],
        newEntry: Entry[K, V]
    ): SMap[K, V] = if (this == oldEntry) newEntry else oldEntry

    override def removeEntry(entry: Entry[K, V]): SMap[K, V] =
      if (this == entry) SMap.empty else this
  }

  final case class KVEntry[K, V](hash: Int, key: K, value: V)
      extends Entry[K, V](hash) {

    override val count: Int = 1

    override def getEntryOrNull(hash: Int, key: K): KVEntry[K, V] =
      if (this.hash == hash && this.key == key) this else null

    override def update(newEntry: KVEntry[K, V]): Entry[K, V] =
      if (this.key == newEntry.key) newEntry
      else HashConflictingEntry(this.hash, Array(newEntry, this))

    override def updateOrKeep[S](
        state: S,
        newEntry: KVEntry[K, V],
        updateOrKeep: UpdaterOrKeeper[S]
    ): Entry[K, V] =
      if (this.key != newEntry.key)
        HashConflictingEntry(this.hash, Array(newEntry, this))
      else if (updateOrKeep(state, this, newEntry) ne this) newEntry
      else this
  }

  private def appendOrReplace[T](
      items: Array[T],
      item: T,
      i: Int = -1
  )(implicit ev: ClassTag[T]): Array[T] = {
    val newItems = new Array[T](if (i != -1) items.length else items.length + 1)
    items.copyToArray(newItems)
    newItems(if (i != -1) i else items.length) = item
    newItems
  }

  case class HashConflictingEntry[K, V](
      hash: Int,
      conflicts: Array[KVEntry[K, V]]
  ) extends Entry[K, V](hash) {

    override val count: Int = conflicts.length

    override def getEntryOrNull(hash: Int, key: K): KVEntry[K, V] =
      conflicts.find(_.key == key).getOrElse(null)

    override def update(newEntry: KVEntry[K, V]): Entry[K, V] = {
      val i = conflicts.indexWhere(_.key == newEntry.key)
      HashConflictingEntry(hash, appendOrReplace(conflicts, newEntry, i))
    }

    override def updateOrKeep[S](
        state: S,
        newEntry: KVEntry[K, V],
        updateOrKeep: UpdaterOrKeeper[S]
    ): Entry[K, V] = {
      val key = newEntry.key
      val i = conflicts.indexWhere(_.key == key)
      if (i == -1)
        HashConflictingEntry(hash, appendOrReplace(conflicts, newEntry))
      else if (updateOrKeep(state, conflicts(i), newEntry) ne conflicts(i))
        HashConflictingEntry(hash, appendOrReplace(conflicts, newEntry, i))
      else this
    }
  }

  final case class Leaf2[K, V](e0: Entry[K, V], e1: Entry[K, V])
      extends SMap[K, V] {}
}
