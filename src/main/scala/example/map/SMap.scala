package example.map

import scala.reflect.ClassTag

/** The type of value `V` is not covariant as in the `Map[K, +V]` because SMap
  * allows to modify the value inside the entry. So in a sense it is
  * semi-immutable data structure which may be used mostly as immutable and
  * additionally to have holes for values to be set or updated later. The value
  * location is fixed in memory so you may grab a reference to the SMap.Entry
  * and update it later without worries of SMap structure additions or
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
  def addOrUpdateEntry(newEntry: KVEntry[K, V]): SMap[K, V] =
    addOrGetEntry(newEntry) match {
      case entry: Entry[K, V] if (entry ne newEntry) =>
        replaceEntry(entry, entry.update(newEntry))
      case newMap => newMap
    }

  def get(key: K): Option[V] = if (isEmpty) None
  else
    getEntryOrNull(key.hashCode) match {
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
  def contains(key: K): Boolean = if (isEmpty) false
  else
    getEntryOrNull(key.hashCode) match {
      case KVEntry(_, k, _) => k == key
      case HashConflictingEntry(_, conflicts) =>
        conflicts.exists(_.key == key)
      case _ => false
    }

  /** Returns the new map without the specified hash and key (if found) or
    * returns the same map otherwise
    */
  def remove(key: K) = if (isEmpty) this
  else
    getEntryOrNull(key.hashCode) match {
      case e: KVEntry[K, V] => removeEntry(e)
      case e: HashConflictingEntry[K, V] => {
        val cs = e.conflicts
        val i = cs.indexWhere(_.key == key)
        if (i != -1) {
          val entryToReplace =
            if (cs.length == 2)
              if (i == 0) cs(1) else cs(0)
            else {
              val newConflicts = new Array[KVEntry[K, V]](cs.length - 1)
              var j = 0
              for (item <- cs if j != i) {
                newConflicts(j) = item
                j += 1
              }
              HashConflictingEntry(e.hash, newConflicts)
            }
          replaceEntry(e, entryToReplace)
        } else this
      }
      case _ => this
    }
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
    ): SMap[K, V] = if (this eq oldEntry) newEntry else oldEntry

    override def removeEntry(entry: Entry[K, V]): SMap[K, V] =
      if (this eq entry) SMap.empty else this
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
      extends SMap[K, V] {

    assert(e0.hash < e1.hash)

    override def size = e0.size + e1.size
    override def getMinHashEntryOrNull = e0
    override def getMaxHashEntryOrNull = e1

    override def getEntryOrNull(hash: Int): Entry[K, V] = hash match {
      case e0.hash => e0
      case e1.hash => e1
      case _       => null
    }

    override def addOrGetEntry(entry: Entry[K, V]): SMap[K, V] =
      entry.hash match {
        case e0.hash => e0
        case e1.hash => e1
        case _       => Leaf2Plus(entry, this)
      }

    override def replaceEntry(
        oldEntry: Entry[K, V],
        newEntry: Entry[K, V]
    ): SMap[K, V] =
      if (oldEntry eq e0) Leaf2(newEntry, e1) else Leaf2(e0, newEntry)

    override protected def removeEntry(entry: Entry[K, V]): SMap[K, V] =
      if (e0 eq entry) e1 else e0
  }

  final case class Leaf2Plus[K, V](p: Entry[K, V], l: Leaf2[K, V])
      extends SMap[K, V] {

    override def size = p.size + l.e0.size + l.e1.size
    override def getMinHashEntryOrNull =
      if (p.hash < l.e0.hash) p else l.e0
    override def getMaxHashEntryOrNull =
      if (p.hash > l.e1.hash) p else l.e1

    override def getEntryOrNull(hash: Int): Entry[K, V] = hash match {
      case p.hash    => p
      case l.e0.hash => l.e0
      case l.e1.hash => l.e1
      case _         => null
    }

    override def addOrGetEntry(entry: Entry[K, V]): SMap[K, V] =
      entry.hash match {
        case p.hash    => p
        case l.e0.hash => l.e0
        case l.e1.hash => l.e1
        case _         => Leaf2PlusPlus(entry, this)
      }

    override def replaceEntry(
        oldEntry: Entry[K, V],
        newEntry: Entry[K, V]
    ): SMap[K, V] =
      if (oldEntry eq p) Leaf2Plus(newEntry, l)
      else if (oldEntry eq l.e0) Leaf2Plus(p, new Leaf2(newEntry, l.e1))
      else Leaf2Plus(p, new Leaf2(l.e0, newEntry))

    override protected def removeEntry(entry: Entry[K, V]): SMap[K, V] =
      if (entry eq p) l
      else if (entry eq l.e0)
        (if (p.hash < l.e1.hash) Leaf2(p, l.e1)
         else new Leaf2(l.e1, p))
      else (if (p.hash < l.e0.hash) Leaf2(p, l.e0)
            else new Leaf2(l.e0, p))
  }

  final case class Leaf2PlusPlus[K, V](p: Entry[K, V], l: Leaf2Plus[K, V])
      extends SMap[K, V] {
    override def size: Int = p.size + l.size

    override def getMinHashEntryOrNull = {
      var m = l.getMinHashEntryOrNull
      if (p.hash < m.hash) p else m
    }

    override def getMaxHashEntryOrNull = {
      var m = l.getMaxHashEntryOrNull
      if (p.hash > m.hash) p else m
    }

    override def getEntryOrNull(hash: Int) =
      if (hash == p.hash) p else l.getEntryOrNull(hash)

    override def addOrGetEntry(entry: Entry[K, V]): SMap[K, V] =
      entry.hash match {
        case p.hash      => p
        case l.p.hash    => l.p
        case l.l.e0.hash => l.l.e0
        case l.l.e1.hash => l.l.e1
        case hash => {
          var e = entry; var pp = l.p; var e0 = l.l.e0; var e1 = l.l.e1
          var p_ = p; val ph = p_.hash; val pph = pp.hash
          var t: Entry[K, V] = null

          // e0 and e1 are already sorted e0 < e1, we need to insert the pp, p, e in the right order,
          // so the result should be e0 < e1 < pp < p < e
          if (pph < e1.hash) {
            t = pp; pp = e1; e1 = t
            if (pph < e0.hash) {
              t = e0; e0 = e1; e1 = t
            }
          }

          if (ph < pp.hash) {
            t = p_; p_ = pp; pp = t
            if (ph < e1.hash) {
              t = pp; pp = e1; e1 = t
              if (ph < e0.hash)
                t = e1; e1 = e0; e0 = t
            }
          }

          if (hash < p.hash) {
            t = e; e = p_; p_ = t
            if (hash < pp.hash) {
              e = p_; p_ = pp; pp = t
              if (hash < e1.hash) {
                t = pp; pp = e1; e1 = t
                if (hash < e0.hash)
                  t = e1; e1 = e0; e0 = t
              }
            }
          }

          Leaf5(e0, e1, pp, p, e);
        }
      }

    override def replaceEntry(
        oldEntry: Entry[K, V],
        newEntry: Entry[K, V]
    ): SMap[K, V] =
      if (oldEntry eq p) Leaf2PlusPlus(newEntry, l)
      else if (oldEntry eq l.p) Leaf2PlusPlus(p, Leaf2Plus(newEntry, l.l))
      else if (oldEntry eq l.l.e0)
        Leaf2PlusPlus(p, Leaf2Plus(l.p, Leaf2(newEntry, l.l.e1)))
      else
        Leaf2PlusPlus(p, Leaf2Plus(l.p, Leaf2(l.l.e0, newEntry)));

    override protected def removeEntry(entry: Entry[K, V]): SMap[K, V] =
      if (entry eq p) l
      else if (entry eq l.p) Leaf2Plus(p, l.l)
      else if (entry eq l.l.e0)
        (if (l.p.hash < l.l.e1.hash) Leaf2Plus(p, Leaf2(l.p, l.l.e1))
         else Leaf2Plus(p, Leaf2(l.l.e1, l.p)))
      else (if (l.p.hash < l.l.e0.hash) Leaf2Plus(p, Leaf2(l.p, l.l.e0))
            else Leaf2Plus(p, Leaf2(l.l.e0, l.p)))
  }

  final case class Leaf5[K, V](
      e0: Entry[K, V],
      e1: Entry[K, V],
      e2: Entry[K, V],
      e3: Entry[K, V],
      e4: Entry[K, V]
  ) extends SMap[K, V] {

    assert(e0.hash < e1.hash)
    assert(e1.hash < e2.hash)
    assert(e2.hash < e3.hash)
    assert(e3.hash < e4.hash)

    override def size = e0.size + e1.size + e2.size + e3.size + e4.size
    override def getMinHashEntryOrNull = e0
    override def getMaxHashEntryOrNull = e4

    override def getEntryOrNull(hash: Int) = hash match {
      case e0.hash => e0
      case e1.hash => e1
      case e2.hash => e2
      case e3.hash => e3
      case e4.hash => e4
      case _       => null
    }

    override def addOrGetEntry(entry: Entry[K, V]): SMap[K, V] =
      entry.hash match {
        case e0.hash => e0
        case e1.hash => e1
        case e2.hash => e2
        case e3.hash => e3
        case e4.hash => e4
        case _       => Leaf5Plus(entry, this)
      }

    override def replaceEntry(
        oldEntry: Entry[K, V],
        newEntry: Entry[K, V]
    ): SMap[K, V] =
      if (oldEntry eq e0) Leaf5(newEntry, e1, e2, e3, e4)
      else if (oldEntry eq e1) Leaf5(e0, newEntry, e2, e3, e4)
      else if (oldEntry eq e2) Leaf5(e0, e1, newEntry, e3, e4)
      else if (oldEntry eq e3) Leaf5(e0, e1, e2, newEntry, e4)
      else
        Leaf5(e0, e1, e2, e3, newEntry)

    override protected def removeEntry(entry: Entry[K, V]): SMap[K, V] =
      if (entry eq e0) Leaf2PlusPlus(e4, Leaf2Plus(e3, Leaf2(e1, e2)))
      else if (entry eq e1) Leaf2PlusPlus(e4, Leaf2Plus(e3, Leaf2(e0, e2)))
      else if (entry eq e2) Leaf2PlusPlus(e4, Leaf2Plus(e3, Leaf2(e0, e1)))
      else if (entry eq e3) Leaf2PlusPlus(e4, Leaf2Plus(e2, Leaf2(e0, e1)))
      else
        Leaf2PlusPlus(e3, Leaf2Plus(e2, Leaf2(e0, e1)))
  }

  final case class Leaf5Plus[K, V](p: Entry[K, V], l: Leaf5[K, V])
      extends SMap[K, V] {

    override def size = p.size + l.size
    override def getMinHashEntryOrNull = if (p.hash < l.e0.hash) p else l.e0
    override def getMaxHashEntryOrNull = if (p.hash > l.e4.hash) p else l.e4

    override def getEntryOrNull(hash: Int): Entry[K, V] = hash match {
      case p.hash    => p
      case l.e0.hash => l.e0
      case l.e1.hash => l.e1
      case l.e2.hash => l.e2
      case l.e3.hash => l.e3
      case l.e4.hash => l.e4
      case _         => null
    }

    override def addOrGetEntry(entry: Entry[K, V]): SMap[K, V] =
      entry.hash match {
        case p.hash    => p
        case l.e0.hash => l.e0
        case l.e1.hash => l.e1
        case l.e2.hash => l.e2
        case l.e3.hash => l.e3
        case l.e4.hash => l.e4
        case _         => Leaf5PlusPlus(entry, this)
      }

    override def replaceEntry(
        oldEntry: Entry[K, V],
        newEntry: Entry[K, V]
    ): SMap[K, V] =
      if (oldEntry eq p)
        Leaf5Plus(newEntry, l)
      else {
        val e0 = l.e0; val e1 = l.e1; val e2 = l.e2; val e3 = l.e3
        val e4 = l.e4
        if (oldEntry eq e0) Leaf5Plus(p, Leaf5(newEntry, e1, e2, e3, e4))
        else if (oldEntry eq e1) Leaf5Plus(p, Leaf5(e0, newEntry, e2, e3, e4))
        else if (oldEntry eq e2) Leaf5Plus(p, Leaf5(e0, e1, newEntry, e3, e4))
        else if (oldEntry eq e3) Leaf5Plus(p, Leaf5(e0, e1, e2, newEntry, e4))
        else Leaf5Plus(p, Leaf5(e0, e1, e2, e3, newEntry))
      }

    override protected def removeEntry(entry: Entry[K, V]): SMap[K, V] =
      if (p eq entry) l
      else {
        var p_ = p; val ph = p_.hash;
        var e0 = l.e0; var e1 = l.e1; var e2 = l.e2; var e3 = l.e3;
        var e4 = l.e4
        var t: Entry[K, V] = null
        if (ph < e4.hash) {
          t = e4; e4 = p_; p_ = t
          if (ph < e3.hash) {
            t = e3; e3 = e4; e4 = t
            if (ph < e2.hash) {
              t = e2; e2 = e3; e3 = t
              if (ph < e1.hash) {
                t = e1; e1 = e2; e2 = t
                if (ph < e0.hash)
                  t = e0; e0 = e1; e1 = t
              }
            }
          }
        }

        if (entry eq e0) Leaf5(e1, e2, e3, e4, p)
        else if (entry eq e1) Leaf5(e0, e2, e3, e4, p)
        else if (entry eq e2) Leaf5(e0, e1, e3, e4, p)
        else if (entry eq e3) Leaf5(e0, e1, e2, e4, p)
        else if (entry eq e4) Leaf5(e0, e1, e2, e3, p)
        else Leaf5(e0, e1, e2, e3, e4)
      }
  }

  final case class Leaf5PlusPlus[K, V](p: Entry[K, V], l: Leaf5Plus[K, V])
      extends SMap[K, V] {}

}
