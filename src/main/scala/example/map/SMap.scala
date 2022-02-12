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

  def size: Int = 0

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
  def addOrGetEntry(entry: Entry[K, V]): SMap[K, V] = entry

  /** Returns the new map with old entry replaced by the new entry. Note that
    * the old entry should be present
    */
  def replaceEntry(
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

  /** Adds or updates (no in-place mutation) the map with the new entry, always
    * returning a new map
    */
  def addOrUpdateEntry(newEntry: KVEntry[K, V]): SMap[K, V] = addOrGetEntry(
    newEntry
  ) match {
    case entry: Entry[K, V] if (entry ne newEntry) =>
      replaceEntry(entry, entry.update(newEntry))
    case newMap => newMap
  }

  def get(key: K): Option[V] = getEntryOrNull(key.hashCode) match {
    case KVEntry(_, k, v) => if (k == key) Some(v) else None
    case HashConflictingEntry(_, conflicts) =>
      conflicts.find(_.key == key).map(_.value)
    case _ => None
  }

  def getOrElse[V1 >: V](key: K, default: => V1): V1 = get(key) match {
    case Some(v) => v
    case None    => default
  }

  @throws[NoSuchElementException]
  def apply(key: K): V = get(key) match {
    case Some(value) => value
    case None        => default(key)
  }

  /** Defines the default value computation for the map, returned when a key is
    * not found. The method implemented here throws an exception, but it might
    * be overridden in subclasses.
    */
  @throws[NoSuchElementException]
  def default(key: K): V =
    throw new NoSuchElementException("key not found: " + key)

  /** Tests whether this map contains a key.
    */
  def contains(key: K): Boolean = get(key).isDefined
}

object SMap {

  private case object Empty extends SMap[Any, Nothing]

  def empty[K, V]: SMap[K, V] = Empty.asInstanceOf[SMap[K, V]]

  def newEntry[K, V](item: (K, V)): KVEntry[K, V] =
    KVEntry(item._1.hashCode, item._1, item._2)

  def apply[K, V](item: (K, V)): SMap[K, V] = newEntry(item)

  def apply[K, V](items: (K, V)*): SMap[K, V] = {
    var m = empty[K, V]
    for (i <- items) m = m.addOrUpdateEntry(newEntry(i))
    m
  }

  abstract class Entry[K, V](val hash: Int) extends SMap[K, V] {

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

    override def addOrGetEntry(entry: Entry[K, V]): SMap[K, V] =
      if (entry.hash > this.hash) new Leaf2(this, entry)
      else if (entry.hash < this.hash) new Leaf2(entry, this)
      else this

    override def replaceEntry(
        oldEntry: Entry[K, V],
        newEntry: Entry[K, V]
    ): SMap[K, V] = if (this == oldEntry) newEntry else oldEntry

    override def removeEntry(entry: Entry[K, V]): SMap[K, V] =
      if (this == entry) SMap.empty else this
  }

  final case class KVEntry[K, V](override val hash: Int, key: K, value: V)
      extends Entry[K, V](hash) {

    override def size: Int = 1

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
      override val hash: Int,
      conflicts: Array[KVEntry[K, V]]
  ) extends Entry[K, V](hash) {

    override def size: Int = conflicts.length

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
