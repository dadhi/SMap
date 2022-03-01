package speedy

import scala.reflect.ClassTag

/** The type of value `V` is not covariant as in the `Map[K, +V]` because SMap
  * allows to modify the value inside the entry. So in a sense it is
  * semi-immutable data structure which may be used mostly as immutable and
  * additionally to have holes for values to be set or updated later. The value
  * location is fixed in memory so you may grab a reference to the SMap.Entry
  * and update it later without worries of SMap structure additions or
  * modifications.
  */
sealed trait SMap[K, V] {
  import SMap._

  /** Indicates that the map is empty
    */
  def isEmpty: Boolean = this eq Empty

  def size: Int = 0

  /** Indicates that the map may grow tot Branch2 and therefore may require
    * re-balance
    */
  protected def closeToSplitAndRebalance = false

  protected def getMinHashEntryOrNull: Entry[K, V] = null
  protected def getMaxHashEntryOrNull: Entry[K, V] = null

  /** Lookup for the entry by hash. If nothing found then the method returns
    * `null`
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
    * the old entry should be present.
    */
  protected def replaceEntry(
      oldEntry: Entry[K, V],
      newEntry: Entry[K, V]
  ): SMap[K, V] = this

  /** Removes the certainly present old entry and returns the new map without
    * the entry.
    */
  protected def removeEntry(entry: Entry[K, V]): SMap[K, V] = this

  /** Creates a new map obtained by updating this map with a given key/value
    * pair.
    */
  def updated(key: K, value: V): SMap[K, V] =
    updated(key, createEntry(key, value))

  def updated(key: K, entry: Entry[K, V]): SMap[K, V] = {
    addOrGetEntry(entry) match {
      case found: Entry[K, V] if (found ne entry) =>
        replaceEntry(found, found.replaceOrAdd(key, entry))
      case newMap => newMap
    }
  }

  /** Alias for `updated`
    */
  def +(kv: (K, V)): SMap[K, V] = updated(kv._1, kv._2)

  /** Returns the new map without the specified hash and key (if found) or
    * returns the same map otherwise
    */
  def removed(key: K): SMap[K, V] = if (isEmpty) this
  else
    getEntryOrNull(key.hashCode) match {
      case e: Entry[K, V] => {
        val entryWithoutKey = e.tryEvict(key)
        if (entryWithoutKey eq e)
          removeEntry(e)
        else
          replaceEntry(e, entryWithoutKey)
      }
      case _ => this
    }
}

object SMap {

  private object Empty extends SMap[Any, Nothing]

  def empty[K, V]: SMap[K, V] = Empty.asInstanceOf[SMap[K, V]]

  @`inline` def createEntry[K, V](key: K, value: V): Entry[K, V] = key match {
    case n: Int => VEntry(n, value).asInstanceOf[Entry[K, V]]
    case _      => KVEntry(key.hashCode, key, value)
  }

  @`inline` def createEntry[V](key: Int, value: V): Entry[Int, V] =
    VEntry(key, value)

  def apply[K, V](item: (K, V)): SMap[K, V] =
    createEntry(item._1, item._2)

  def apply[V](item: (Int, V))(implicit d: DummyImplicit): SMap[Int, V] =
    createEntry(item._1, item._2)

  def apply[K, V](items: (K, V)*): SMap[K, V] = {
    var m = empty[K, V]
    for ((key, value) <- items)
      m = m.updated(key, createEntry(key, value))
    m
  }

  def apply[V](items: (Int, V)*)(implicit d: DummyImplicit): SMap[Int, V] = {
    var m = empty[Int, V]
    for ((key, value) <- items)
      m = m.updated(key, createEntry(key, value))
    m
  }

  @throws[NoSuchElementException]
  def default[K](key: K): Nothing =
    throw new NoSuchElementException("key not found: " + key)

  final class MapParentStack(capacity: Int = 4) {

    var _items: Array[AnyVal] = new Array[AnyVal](capacity)

    def put(item: AnyVal, index: Int): Unit = {
      if (index >= _items.size) expandItems()
      _items(index) = item
    }

    def apply(index: Int) = _items(index)

    private def expandItems(): Unit = {
      val size = _items.size
      val newItems = new Array[AnyVal](size << 1) // count * 2
      _items.copyToArray(newItems)
      _items = newItems
    }
  }

  implicit class Extensions[K, V](val map: SMap[K, V]) extends AnyVal {
    def get(key: K): Option[V] =
      map.getEntryOrNull(key.hashCode) match {
        case e: Entry[K, V] => e.get(key)
        case _              => None
      }

    def getOrElse[V1 >: V](key: K, default: => V1): V1 =
      get(key).getOrElse(default)

    /** Tests whether this map contains a key.
      */
    def contains(key: K): Boolean = get(key).isDefined

    /** Defines the default value computation for the map, returned when a key
      * is not found. The method implemented here throws an exception, but it
      * might be overridden in subclasses.
      */
    @throws[NoSuchElementException]
    def apply(key: K): V =
      get(key) match {
        case Some(value) => value
        case None        => default(key)
      }

    /** Update a mapping for the specified key and its current optionally-mapped
      * value (`Some` if there is current mapping, `None` if not).
      *
      * If the remapping function returns `Some(v)`, the mapping is updated with
      * the new value `v`. If the remapping function returns `None`, the mapping
      * is removed (or remains absent if initially absent). If the function
      * itself throws an exception, the exception is re-thrown, and the current
      * mapping is left unchanged.
      */
    def updatedWith(
        key: K
    )(
        remappingFunction: Option[V] => Option[V]
    ): SMap[K, V] = {
      val oldValue = get(key)
      val newValue = remappingFunction(oldValue)
      (oldValue, newValue) match {
        case (None, None)    => map
        case (Some(_), None) => map.removed(key)
        case (_, Some(v))    => map.updated(key, v)
      }
    }

    /** Depth-first in-order of hash traversal as described in
      * http://en.wikipedia.org/wiki/Tree_traversal. The `parents` parameter
      * allows to reuse the stack memory used for the traversal between multiple
      * calls. So you may pass the empty `parents` into the first `Enumerate`
      * and then keep passing the same `parents` into the subsequent calls
      */
    def foreach[S](state: S, parents: MapParentStack = null)(
        handler: (Entry[K, V], Int, S) => Unit
    ): S = {
      if (!map.isEmpty) {
        var i = 0;
        if (map.isInstanceOf[Entry[K, V]]) {
          map match {
            case kv: KVEntry[K, V] => handler(kv, i, state)
            case hc: HashConflictingEntry[K, V] =>
              for (c <- hc.conflicts) {
                handler(c, i, state)
                i += 1
              }
            case _ => ()
          }
        } else {
          ???
        }
        // {
        //     if (e is ImHashMapEntry<K, V> kv) handler(kv, 0, state);
        //     else foreach (var c in ((HashConflictingEntry<K, V>)e).Conflicts) handler(c, i++, state);
        //     return state;
        // }

        ???
      }
      state
    }
    // /// <summary>
    // /// Depth-first in-order of hash traversal as described in http://en.wikipedia.org/wiki/Tree_traversal.
    // /// The `parents` parameter allows to reuse the stack memory used for the traversal between multiple calls.
    // /// So you may pass the empty `parents` into the first `Enumerate` and then keep passing the same `parents` into the subsequent calls</summary>
    // public static S ForEach<K, V, S>(this ImHashMap<K, V> map, S state, Action<ImHashMapEntry<K, V>, int, S> handler, MapParentStack parents = null)
    // {
    //     if (map == ImHashMap<K, V>.Empty)
    //         return state;
    //     var i = 0;
    //     if (map is ImHashMap<K, V>.Entry e)
    //     {
    //         if (e is ImHashMapEntry<K, V> kv) handler(kv, 0, state);
    //         else foreach (var c in ((HashConflictingEntry<K, V>)e).Conflicts) handler(c, i++, state);
    //         return state;
    //     }

    //     var count = 0;
    //     while (true)
    //     {
    //         if (map is ImHashMap<K, V>.Branch2 b2)
    //         {
    //             if (parents == null)
    //                 parents = new MapParentStack();
    //             parents.Put(map, count++);
    //             map = b2.Left;
    //             continue;
    //         }
    //         if (map is ImHashMap<K, V>.Branch3 b3)
    //         {
    //             if (parents == null)
    //                 parents = new MapParentStack();
    //             parents.Put(map, count++);
    //             map = b3.Left;
    //             continue;
    //         }

    //         if (map is ImHashMap<K, V>.Entry l1)
    //         {
    //             if (l1 is ImHashMapEntry<K, V> v0) handler(v0, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)l1).Conflicts) handler(c, i++, state);
    //         }
    //         else if (map is ImHashMap<K, V>.Leaf2 l2)
    //         {
    //             if (l2.Entry0 is ImHashMapEntry<K, V> v0) handler(v0, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)l2.Entry0).Conflicts) handler(c, i++, state);
    //             if (l2.Entry1 is ImHashMapEntry<K, V> v1) handler(v1, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)l2.Entry1).Conflicts) handler(c, i++, state);
    //         }
    //         else if (map is ImHashMap<K, V>.Leaf2Plus1 l21)
    //         {
    //             var p  = l21.Plus;
    //             var ph = p.Hash;
    //             var l  = l21.L;
    //             ImHashMap<K, V>.Entry e0 = l.Entry0, e1 = l.Entry1, swap = null;
    //             if (ph < e1.Hash)
    //             {
    //                 swap = e1; e1 = p; p = swap;
    //                 if (ph < e0.Hash)
    //                 {
    //                     swap = e0; e0 = e1; e1 = swap;
    //                 }
    //             }

    //             if (e0 is ImHashMapEntry<K, V> v0) handler(v0, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e0).Conflicts) handler(c, i++, state);
    //             if (e1 is ImHashMapEntry<K, V> v1) handler(v1, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e1).Conflicts) handler(c, i++, state);
    //             if (p  is ImHashMapEntry<K, V> v2) handler(v2, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)p ).Conflicts) handler(c, i++, state);
    //         }
    //         else if (map is ImHashMap<K, V>.Leaf2Plus1Plus1 l211)
    //         {
    //             var p  = l211.Plus;
    //             var pp = l211.L.Plus;
    //             var ph = pp.Hash;
    //             var l  = l211.L.L;
    //             ImHashMap<K, V>.Entry e0 = l.Entry0, e1 = l.Entry1, swap = null;
    //             if (ph < e1.Hash)
    //             {
    //                 swap = e1; e1 = pp; pp = swap;
    //                 if (ph < e0.Hash)
    //                 {
    //                     swap = e0; e0 = e1; e1 = swap;
    //                 }
    //             }

    //             ph = p.Hash;
    //             if (ph < pp.Hash)
    //             {
    //                 swap = pp; pp = p; p = swap;
    //                 if (ph < e1.Hash)
    //                 {
    //                     swap = e1; e1 = pp; pp = swap;
    //                     if (ph < e0.Hash)
    //                     {
    //                         swap = e0; e0 = e1; e1 = swap;
    //                     }
    //                 }
    //             }

    //             if (e0 is ImHashMapEntry<K, V> v0) handler(v0, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e0).Conflicts) handler(c, i++, state);
    //             if (e1 is ImHashMapEntry<K, V> v1) handler(v1, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e1).Conflicts) handler(c, i++, state);
    //             if (pp is ImHashMapEntry<K, V> v2) handler(v2, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)pp).Conflicts) handler(c, i++, state);
    //             if (p  is ImHashMapEntry<K, V> v3) handler(v3, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)p).Conflicts)  handler(c, i++, state);
    //         }
    //         else if (map is ImHashMap<K, V>.Leaf5 l5)
    //         {
    //             if (l5.Entry0 is ImHashMapEntry<K, V> v0) handler(v0, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)l5.Entry0).Conflicts) handler(c, i++, state);
    //             if (l5.Entry1 is ImHashMapEntry<K, V> v1) handler(v1, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)l5.Entry1).Conflicts) handler(c, i++, state);
    //             if (l5.Entry2 is ImHashMapEntry<K, V> v2) handler(v2, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)l5.Entry2).Conflicts) handler(c, i++, state);
    //             if (l5.Entry3 is ImHashMapEntry<K, V> v3) handler(v3, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)l5.Entry3).Conflicts) handler(c, i++, state);
    //             if (l5.Entry4 is ImHashMapEntry<K, V> v4) handler(v4, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)l5.Entry4).Conflicts) handler(c, i++, state);
    //         }
    //         else if (map is ImHashMap<K, V>.Leaf5Plus1 l51)
    //         {
    //             var p  = l51.Plus;
    //             var ph = p.Hash;
    //             var l  = l51.L;
    //             ImHashMap<K, V>.Entry e0 = l.Entry0, e1 = l.Entry1, e2 = l.Entry2, e3 = l.Entry3, e4 = l.Entry4, swap = null;
    //             if (ph < e4.Hash)
    //             {
    //                 swap = e4; e4 = p; p = swap;
    //                 if (ph < e3.Hash)
    //                 {
    //                     swap = e3; e3 = e4; e4 = swap;
    //                     if (ph < e2.Hash)
    //                     {
    //                         swap = e2; e2 = e3; e3 = swap;
    //                         if (ph < e1.Hash)
    //                         {
    //                             swap = e1; e1 = e2; e2 = swap;
    //                             if (ph < e0.Hash)
    //                             {
    //                                 swap = e0; e0 = e1; e1 = swap;
    //                             }
    //                         }
    //                     }
    //                 }
    //             }

    //             if (e0 is ImHashMapEntry<K, V> v0) handler(v0, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e0).Conflicts) handler(c, i++, state);
    //             if (e1 is ImHashMapEntry<K, V> v1) handler(v1, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e1).Conflicts) handler(c, i++, state);
    //             if (e2 is ImHashMapEntry<K, V> v2) handler(v2, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e2).Conflicts) handler(c, i++, state);
    //             if (e3 is ImHashMapEntry<K, V> v3) handler(v3, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e3).Conflicts) handler(c, i++, state);
    //             if (e4 is ImHashMapEntry<K, V> v4) handler(v4, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e4).Conflicts) handler(c, i++, state);
    //             if (p  is ImHashMapEntry<K, V> v5) handler(v5, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)p).Conflicts)  handler(c, i++, state);
    //         }
    //         else if (map is ImHashMap<K, V>.Leaf5Plus1Plus1 l511)
    //         {
    //             var l = l511.L.L;
    //             ImHashMap<K, V>.Entry
    //                 e0 = l.Entry0, e1 = l.Entry1, e2 = l.Entry2, e3 = l.Entry3, e4 = l.Entry4, p = l511.Plus, pp = l511.L.Plus, swap = null;
    //             var h = pp.Hash;
    //             if (h < e4.Hash)
    //             {
    //                 swap = e4; e4 = pp; pp = swap;
    //                 if (h < e3.Hash)
    //                 {
    //                     swap = e3; e3 = e4; e4 = swap;
    //                     if (h < e2.Hash)
    //                     {
    //                         swap = e2; e2 = e3; e3 = swap;
    //                         if (h < e1.Hash)
    //                         {
    //                             swap = e1; e1 = e2; e2 = swap;
    //                             if (h < e0.Hash)
    //                             {
    //                                 swap = e0; e0 = e1; e1 = swap;
    //                             }
    //                         }
    //                     }
    //                 }
    //             }

    //             h = p.Hash;
    //             if (h < pp.Hash)
    //             {
    //                 swap = pp; pp = p; p = swap;
    //                 if (h < e4.Hash)
    //                 {
    //                     swap = e4; e4 = pp; pp = swap;
    //                     if (h < e3.Hash)
    //                     {
    //                         swap = e3; e3 = e4; e4 = swap;
    //                         if (h < e2.Hash)
    //                         {
    //                             swap = e2; e2 = e3; e3 = swap;
    //                             if (h < e1.Hash)
    //                             {
    //                                 swap = e1; e1 = e2; e2 = swap;
    //                                 if (h < e0.Hash)
    //                                 {
    //                                     swap = e0; e0 = e1; e1 = swap;
    //                                 }
    //                             }
    //                         }
    //                     }
    //                 }
    //             }

    //             if (e0 is ImHashMapEntry<K, V> v0) handler(v0, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e0).Conflicts) handler(c, i++, state);
    //             if (e1 is ImHashMapEntry<K, V> v1) handler(v1, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e1).Conflicts) handler(c, i++, state);
    //             if (e2 is ImHashMapEntry<K, V> v2) handler(v2, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e2).Conflicts) handler(c, i++, state);
    //             if (e3 is ImHashMapEntry<K, V> v3) handler(v3, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e3).Conflicts) handler(c, i++, state);
    //             if (e4 is ImHashMapEntry<K, V> v4) handler(v4, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)e4).Conflicts) handler(c, i++, state);
    //             if (pp is ImHashMapEntry<K, V> v5) handler(v5, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)pp).Conflicts) handler(c, i++, state);
    //             if (p  is ImHashMapEntry<K, V> v6) handler(v6, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)p).Conflicts)  handler(c, i++, state);
    //         }

    //         if (count == 0)
    //             break; // we yield the leaf and there is nothing in stack - we are DONE!

    //         var b = parents.Get(--count); // otherwise get the parent
    //         if (b is ImHashMap<K,V>.Branch2 pb2)
    //         {
    //             if (pb2.MidEntry is ImHashMapEntry<K, V> v) handler(v, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)pb2.MidEntry).Conflicts) handler(c, i++, state);
    //             map = pb2.Right;
    //         }
    //         else if (b != _enumerationB3Tombstone)
    //         {
    //             var pb3 = (ImHashMap<K, V>.Branch3)b;
    //             if (pb3.Entry0 is ImHashMapEntry<K, V> v) handler(v, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)pb3.Entry0).Conflicts) handler(c, i++, state);
    //             map = pb3.Middle;
    //             parents.Put(_enumerationB3Tombstone, ++count);
    //             ++count;
    //         }
    //         else
    //         {
    //             var pb3 = (ImHashMap<K, V>.Branch3)parents.Get(--count);
    //             if (pb3.Entry1 is ImHashMapEntry<K, V> v) handler(v, i++, state);
    //             else foreach (var c in ((HashConflictingEntry<K, V>)pb3.Entry1).Conflicts) handler(c, i++, state);
    //             map = pb3.Right;
    //         }
    //     }

    //     return state;
    // }

  }

  implicit class IntKeyExtensions[V](val map: SMap[Int, V]) extends AnyVal {
    def get(key: Int): Option[V] =
      map.getEntryOrNull(key) match {
        case e: VEntry[V] => Some(e.value)
        case _            => None
      }

    def getOrElse[V1 >: V](key: Int, default: => V1): V1 =
      get(key).getOrElse(default)

    /** Tests whether this map contains a key.
      */
    def contains(key: Int): Boolean = get(key).isDefined

    /** Defines the default value computation for the map, returned when a key
      * is not found. The method implemented here throws an exception, but it
      * might be overridden in subclasses.
      */
    @throws[NoSuchElementException]
    def apply(key: Int): V =
      map.getEntryOrNull(key) match {
        case e: VEntry[V] => e.value
        case _            => default(key)
      }
  }

  protected abstract class Entry[K, V](val hash: Int) extends SMap[K, V] {

    override def size: Int = 1
    override def getMinHashEntryOrNull: Entry[K, V] = this
    override def getMaxHashEntryOrNull: Entry[K, V] = this
    override def getEntryOrNull(hash: Int): Entry[K, V] =
      if (hash == this.hash) this else null

    /** Get the value if the `key` is matching the one stored in the entry
      */
    def get(key: K): Option[V]

    /** Updating the entry with the new one. It cannot return an empty map.
      */
    def replaceOrAdd(key: K, entry: Entry[K, V]): Entry[K, V]

    /** Abstracts eviction of the key from the entry. Normally it will keep the
      * entry because the key is part of it. But for multi-key entry it produce
      * another entry without the key.
      */
    def tryEvict(key: K): Entry[K, V] = this

    // /** Updating the entry with the new one using the `updateOrKeep` method
    //   */
    // def updateOrKeep[S](
    //     state: S,
    //     newEntry: KVEntry[K, V],
    //     updateOrKeep: UpdaterOrKeeper[S]
    // ): Entry[K, V]

    /** The method won't add the entry in the case of the conflicting hash here,
      * because this is the responsibility of `replaceEntry`. So you may always
      * expect the same entry to be returned for the conflicting hash.
      */
    override def addOrGetEntry(entry: Entry[K, V]): SMap[K, V] =
      if (entry.hash > hash) new Leaf2(this, entry)
      else if (entry.hash < hash) new Leaf2(entry, this)
      else this

    /** When down to the entry, the oldEntry should be present in the entry
      */
    override def replaceEntry(oldEntry: Entry[K, V], newEntry: Entry[K, V]) = {
      assert(this eq oldEntry)
      newEntry
    }

    /** When down to the entry, the entry should be present in the entry
      */
    override def removeEntry(entry: Entry[K, V]): SMap[K, V] = {
      assert(this eq entry)
      SMap.empty
    }
  }

  final case class VEntry[V](override val hash: Int, value: V)
      extends Entry[Int, V](hash) {

    override def get(key: Int): Option[V] =
      if (key == hash) Some(value) else None

    override def replaceOrAdd(key: Int, entry: Entry[Int, V]): Entry[Int, V] =
      entry

    //   override def updateOrKeep[S](
    //       state: S,
    //       newEntry: KVEntry[K, V],
    //       updateOrKeep: UpdaterOrKeeper[S]
    //   ): Entry[K, V] =
    //     if (this.key != newEntry.key)
    //       HashConflictingEntry(this.hash, Array(newEntry, this))
    //     else if (updateOrKeep(state, this, newEntry) ne this) newEntry
    //     else this
  }

  final case class KVEntry[K, V](override val hash: Int, key: K, value: V)
      extends Entry[K, V](hash) {

    override def get(key: K): Option[V] =
      if (this.key == key) Some(value) else None

    override def replaceOrAdd(key: K, entry: Entry[K, V]): Entry[K, V] =
      if (this.key == key) entry
      else {
        val newEntry = entry.asInstanceOf[KVEntry[K, V]]
        HashConflictingEntry(hash, Array(newEntry, this))
      }

    //   override def updateOrKeep[S](
    //       state: S,
    //       newEntry: KVEntry[K, V],
    //       updateOrKeep: UpdaterOrKeeper[S]
    //   ): Entry[K, V] =
    //     if (this.key != newEntry.key)
    //       HashConflictingEntry(this.hash, Array(newEntry, this))
    //     else if (updateOrKeep(state, this, newEntry) ne this) newEntry
    //     else this
  }

  private def appendOrReplace[T](items: Array[T], item: T, i: Int)(implicit
      ev: ClassTag[T]
  ): Array[T] = {
    val newItems = new Array[T](if (i != -1) items.length else items.length + 1)
    items.copyToArray(newItems)
    newItems(if (i != -1) i else items.length) = item
    newItems
  }

  final case class HashConflictingEntry[K, V](
      override val hash: Int,
      conflicts: Array[KVEntry[K, V]]
  ) extends Entry[K, V](hash) {

    override def size: Int = conflicts.length

    override def get(key: K): Option[V] =
      conflicts.find(_.key == key).map(_.value)

    override def replaceOrAdd(key: K, entry: Entry[K, V]): Entry[K, V] = {
      val i = conflicts.indexWhere(_.key == key)
      // todo: @safety try to remove asInstanceOf here
      val newEntry = entry.asInstanceOf[KVEntry[K, V]]
      copy(conflicts = appendOrReplace(conflicts, newEntry, i))
    }

    override def tryEvict(key: K): Entry[K, V] = {
      val cs = conflicts
      val i = cs.indexWhere(_.key == key)
      if (i == -1) this
      else {
        if (cs.length == 2)
          if (i == 0) cs(1) else cs(0)
        else {
          val newConflicts = new Array[KVEntry[K, V]](cs.length - 1)
          var j = 0
          for (item <- cs if j != i) {
            newConflicts(j) = item
            j += 1
          }
          HashConflictingEntry(hash, newConflicts)
        }
      }
    }

    //   override def updateOrKeep[S](
    //       state: S,
    //       newEntry: KVEntry[K, V],
    //       updateOrKeep: UpdaterOrKeeper[S]
    //   ): Entry[K, V] = {
    //     val key = newEntry.key
    //     val i = conflicts.indexWhere(_.key == key)
    //     if (i == -1)
    //       HashConflictingEntry(hash, appendOrReplace(conflicts, newEntry))
    //     else if (updateOrKeep(state, conflicts(i), newEntry) ne conflicts(i))
    //       HashConflictingEntry(hash, appendOrReplace(conflicts, newEntry, i))
    //     else this
    //   }
  }

  protected final case class Leaf2[K, V](e0: Entry[K, V], e1: Entry[K, V])
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

  protected final case class Leaf2Plus[K, V](p: Entry[K, V], l: Leaf2[K, V])
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

    override def addOrGetEntry(entry: Entry[K, V]): SMap[K, V] = {
      val found = getEntryOrNull(entry.hash)
      if (found ne null) found else Leaf2PlusPlus(entry, this)
    }

    override def replaceEntry(
        oldEntry: Entry[K, V],
        newEntry: Entry[K, V]
    ): SMap[K, V] =
      if (oldEntry eq p) Leaf2Plus(newEntry, l)
      else if (oldEntry eq l.e0) Leaf2Plus(p, new Leaf2(newEntry, l.e1))
      else Leaf2Plus(p, new Leaf2(l.e0, newEntry))

    override def removeEntry(entry: Entry[K, V]): SMap[K, V] =
      if (entry eq p) l
      else if (entry eq l.e0)
        (if (p.hash < l.e1.hash) Leaf2(p, l.e1)
         else new Leaf2(l.e1, p))
      else (if (p.hash < l.e0.hash) Leaf2(p, l.e0)
            else new Leaf2(l.e0, p))
  }

  protected final case class Leaf2PlusPlus[K, V](
      p: Entry[K, V],
      l: Leaf2Plus[K, V]
  ) extends SMap[K, V] {
    override def size: Int = p.size + l.size

    override def getMinHashEntryOrNull = {
      val m = l.getMinHashEntryOrNull
      if (p.hash < m.hash) p else m
    }

    override def getMaxHashEntryOrNull = {
      val m = l.getMaxHashEntryOrNull
      if (p.hash > m.hash) p else m
    }

    override def getEntryOrNull(hash: Int) =
      if (hash == p.hash) p else l.getEntryOrNull(hash)

    override def addOrGetEntry(entry: Entry[K, V]): SMap[K, V] = {
      val found = getEntryOrNull(entry.hash)
      if (found ne null) found
      else {
        var lp = l.p; var e0 = l.l.e0; var e1 = l.l.e1
        var p_ = p; val ph = p_.hash; val lph = lp.hash
        var t: Entry[K, V] = null

        // e0 and e1 are already sorted e0 < e1, we need to insert the lp, p_, e in the right order,
        // so the result should be e0 < e1 < lp < p < e
        if (lph < e1.hash) {
          t = lp; lp = e1; e1 = t
          if (lph < e0.hash) {
            t = e0; e0 = e1; e1 = t
          }
        }

        if (ph < lp.hash) {
          t = p_; p_ = lp; lp = t
          if (ph < e1.hash) {
            t = lp; lp = e1; e1 = t
            if (ph < e0.hash)
              t = e1; e1 = e0; e0 = t
          }
        }

        var e = entry; val hash = e.hash
        if (hash < p_.hash) {
          t = e; e = p_; p_ = t
          if (hash < lp.hash) {
            e = p_; p_ = lp; lp = t
            if (hash < e1.hash) {
              t = lp; lp = e1; e1 = t
              if (hash < e0.hash)
                t = e1; e1 = e0; e0 = t
            }
          }
        }

        Leaf5(e0, e1, lp, p_, e)
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
        Leaf2PlusPlus(p, Leaf2Plus(l.p, Leaf2(l.l.e0, newEntry)))

    override protected def removeEntry(entry: Entry[K, V]): SMap[K, V] =
      if (entry eq p) l
      else if (entry eq l.p) Leaf2Plus(p, l.l)
      else if (entry eq l.l.e0)
        (if (l.p.hash < l.l.e1.hash) Leaf2Plus(p, Leaf2(l.p, l.l.e1))
         else Leaf2Plus(p, Leaf2(l.l.e1, l.p)))
      else (if (l.p.hash < l.l.e0.hash) Leaf2Plus(p, Leaf2(l.p, l.l.e0))
            else Leaf2Plus(p, Leaf2(l.l.e0, l.p)))
  }

  protected final case class Leaf5[K, V](
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

    override def addOrGetEntry(entry: Entry[K, V]): SMap[K, V] = {
      val found = getEntryOrNull(entry.hash)
      if (found ne null) found else Leaf5Plus(entry, this)
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

  protected final case class Leaf5Plus[K, V](p: Entry[K, V], l: Leaf5[K, V])
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

    override def addOrGetEntry(entry: Entry[K, V]): SMap[K, V] = {
      val found = getEntryOrNull(entry.hash)
      if (found ne null) found else Leaf5PlusPlus(entry, this)
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

    override def removeEntry(entry: Entry[K, V]): SMap[K, V] =
      if (p eq entry) l
      else {
        var p_ = p; val ph = p_.hash
        var e0 = l.e0; var e1 = l.e1; var e2 = l.e2; var e3 = l.e3
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

        if (entry eq e0) Leaf5(e1, e2, e3, e4, p_)
        else if (entry eq e1) Leaf5(e0, e2, e3, e4, p_)
        else if (entry eq e2) Leaf5(e0, e1, e3, e4, p_)
        else if (entry eq e3) Leaf5(e0, e1, e2, e4, p_)
        else if (entry eq e4) Leaf5(e0, e1, e2, e3, p_)
        else Leaf5(e0, e1, e2, e3, e4)
      }
  }

  protected final case class Leaf5PlusPlus[K, V](
      p: Entry[K, V],
      l: Leaf5Plus[K, V]
  ) extends SMap[K, V] {
    override def closeToSplitAndRebalance = true

    override def size = p.size + l.p.size + l.l.size

    override def getMinHashEntryOrNull = {
      val m = l.getMinHashEntryOrNull
      if (p.hash < m.hash) p else m
    }

    override def getMaxHashEntryOrNull = {
      val m = l.getMaxHashEntryOrNull
      if (p.hash > m.hash) p else m
    }

    override def getEntryOrNull(hash: Int) =
      if (hash == p.hash) p
      else if (hash == l.p.hash) l.p
      else l.l.getEntryOrNull(hash)

    override def addOrGetEntry(entry: Entry[K, V]) = {
      val hash = entry.hash
      val found = getEntryOrNull(hash)
      if (found ne null) found
      else {
        val ll = l.l
        var e0 = ll.e0; var e1 = ll.e1; var e2 = ll.e2; var e3 = ll.e3
        var e4 = ll.e4
        var p_ = p; val ph = p_.hash; var lp = l.p; val lph = lp.hash

        val right = hash > e4.hash && ph > e4.hash && lph > e4.hash
        val left = !right && hash < e0.hash && ph < e0.hash && lph < e0.hash

        var t: Entry[K, V] = null
        if (lph < e4.hash) {
          t = e4; e4 = lp; lp = t
          if (lph < e3.hash) {
            t = e3; e3 = e4; e4 = t
            if (lph < e2.hash) {
              t = e2; e2 = e3; e3 = t
              if (lph < e1.hash) {
                t = e1; e1 = e2; e2 = t
                if (lph < e0.hash)
                  t = e0; e0 = e1; e1 = t
              }
            }
          }
        }

        if (ph < lp.hash) {
          t = lp; lp = p_; p_ = t
          if (ph < e4.hash) {
            t = e4; e4 = lp; lp = t
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
        }

        var e = entry
        if (hash < p_.hash) {
          t = p_; p_ = e; e = t
          if (hash < lp.hash) {
            t = lp; lp = p_; p_ = t
            if (hash < e4.hash) {
              t = e4; e4 = lp; lp = t
              if (hash < e3.hash) {
                t = e3; e3 = e4; e4 = t
                if (hash < e2.hash) {
                  t = e2; e2 = e3; e3 = t
                  if (hash < e1.hash) {
                    t = e1; e1 = e2; e2 = t
                    if (hash < e0.hash)
                      t = e0; e0 = e1; e1 = t
                  }
                }
              }
            }
          }
        }

        if (left) Branch2(Leaf2(e0, e1), e2, l)
        else if (right) Branch2(ll, lp, Leaf2(p_, e))
        else Branch2(Leaf5(e0, e1, e2, e3, e4), lp, Leaf2(p_, e))
      }
    }

    override def replaceEntry(oldEntry: Entry[K, V], newEntry: Entry[K, V]) = {
      if (p eq oldEntry)
        Leaf5PlusPlus(newEntry, l)
      else if (l.p eq oldEntry)
        Leaf5PlusPlus(p, new Leaf5Plus(newEntry, l.l))
      else {
        val lp = l.p; val ll = l.l
        val e0 = ll.e0; val e1 = ll.e1; val e2 = ll.e2; val e3 = ll.e3
        val e4 = ll.e4
        if (oldEntry eq e0)
          Leaf5PlusPlus(
            p,
            new Leaf5Plus(lp, new Leaf5(newEntry, e1, e2, e3, e4))
          )
        else if (oldEntry eq e1)
          Leaf5PlusPlus(
            p,
            new Leaf5Plus(lp, new Leaf5(e0, newEntry, e2, e3, e4))
          )
        else if (oldEntry eq e2)
          Leaf5PlusPlus(
            p,
            new Leaf5Plus(lp, new Leaf5(e0, e1, newEntry, e3, e4))
          )
        else if (oldEntry eq e3)
          Leaf5PlusPlus(
            p,
            new Leaf5Plus(lp, new Leaf5(e0, e1, e2, newEntry, e4))
          )
        else
          Leaf5PlusPlus(
            p,
            new Leaf5Plus(lp, new Leaf5(e0, e1, e2, e3, newEntry))
          )
      }
    }

    override protected def removeEntry(entry: Entry[K, V]) = {
      if (p eq entry) l
      else if (l.p eq entry) Leaf5Plus(p, l.l)
      else {
        var p_ = p; var lp = l.p
        val ll = l.l; val lph = lp.hash; val ph = p.hash
        var e0 = ll.e0; var e1 = ll.e1; var e2 = ll.e2; var e3 = ll.e3
        var e4 = ll.e4
        var t: Entry[K, V] = null
        if (lph < e4.hash) {
          t = e4; e4 = lp; lp = t
          if (lph < e3.hash) {
            t = e3; e3 = e4; e4 = t
            if (lph < e2.hash) {
              t = e2; e2 = e3; e3 = t
              if (lph < e1.hash) {
                t = e1; e1 = e2; e2 = t
                if (lph < e0.hash) {
                  t = e0; e0 = e1; e1 = t
                }
              }
            }
          }
        }
        if (ph < lp.hash) {
          t = lp; lp = p_; p_ = t
          if (ph < e4.hash) {
            t = e4; e4 = lp; lp = t
            if (ph < e3.hash) {
              t = e3; e3 = e4; e4 = t
              if (ph < e2.hash) {
                t = e2; e2 = e3; e3 = t
                if (ph < e1.hash) {
                  t = e1; e1 = e2; e2 = t
                  if (ph < e0.hash) {
                    t = e0; e0 = e1; e1 = t
                  }
                }
              }
            }
          }
        }
        if (entry eq e0) Leaf5Plus(p_, new Leaf5(e1, e2, e3, e4, lp))
        else if (entry eq e1) Leaf5Plus(p_, new Leaf5(e0, e2, e3, e4, lp))
        else if (entry eq e2) Leaf5Plus(p_, new Leaf5(e0, e1, e3, e4, lp))
        else if (entry eq e3) Leaf5Plus(p_, new Leaf5(e0, e1, e2, e4, lp))
        else if (entry eq e4) Leaf5Plus(p_, new Leaf5(e0, e1, e2, e3, lp))
        else if (entry eq lp) Leaf5Plus(p_, new Leaf5(e0, e1, e2, e3, e4))
        else Leaf5Plus(lp, new Leaf5(e0, e1, e2, e3, e4))
      }
    }
  }

  /** Branch of 2 leafs or branches with entry in the middle
    */
  protected final case class Branch2[K, V](
      left: SMap[K, V],
      e: Entry[K, V],
      right: SMap[K, V]
  ) extends SMap[K, V] {

    assert(!left.isEmpty && !right.isEmpty)

    override def size = left.size + e.size + right.size
    override def getMinHashEntryOrNull = left.getMinHashEntryOrNull
    override def getMaxHashEntryOrNull = right.getMaxHashEntryOrNull

    override def getEntryOrNull(hash: Int) =
      if (hash > e.hash) right.getEntryOrNull(hash)
      else if (hash < e.hash) left.getEntryOrNull(hash)
      else e

    override def addOrGetEntry(entry: Entry[K, V]) = {
      val hash = entry.hash
      if (hash > e.hash)
        right.addOrGetEntry(entry) match {
          case b2: Branch2[K, V] if (right.closeToSplitAndRebalance) =>
            Branch3(left, e, b2.left, b2.e, b2.right)
          // it is fine to return the possibly added entry (e.g. `HashConflictingEntry`)
          // because on the consumer side we will call the `replaceEntry` for such case.
          case found: Entry[K, V] => found
          case added              => Branch2(left, e, added)
        }
      else if (hash < e.hash) {
        left.addOrGetEntry(entry) match {
          case b2: Branch2[K, V] if (left.closeToSplitAndRebalance) =>
            Branch3(b2.left, b2.e, b2.right, e, right)
          case found: Entry[K, V] => found
          case added              => Branch2(added, e, right)
        }
      } else e
    }

    override def replaceEntry(oldEntry: Entry[K, V], newEntry: Entry[K, V]) =
      if (oldEntry eq e)
        Branch2(left, newEntry, right)
      else if (oldEntry.hash > e.hash)
        Branch2(left, e, right.replaceEntry(oldEntry, newEntry))
      else
        Branch2(left.replaceEntry(oldEntry, newEntry), e, right)

    override def removeEntry(entry: Entry[K, V]) = {
      // The downward phase for deleting an element from a 2-3 tree is the same as the downward phase
      // for inserting an element except for the case when the element to be deleted is equal to the value in
      // a 2-node or a 3-node. In this case, if the value is not part of a terminal node, the value is replaced
      // by its in-order predecessor or in-order successor, just as in binary search tree deletion. So in any
      // case, deletion leaves a hole in a terminal node.
      // The goal of the rest of the deletion algorithm is to remove the hole without violating the other
      // invariants of the 2-3 tree.

      if (entry.hash > e.hash) {
        val newRight = right.removeEntry(entry)
        right match {
          // the Entry means that we are removing the right completely.
          case _: Entry[K, V] =>
            // if the left node is not a full leaf yet then merge into the leaf,
            // going from the branch to leaf and decreasing the height by one.
            if (!left.closeToSplitAndRebalance)
              left.addOrGetEntry(e)
            else {
              val maxLeftEntry = left.getMaxHashEntryOrNull
              Branch2(left.removeEntry(maxLeftEntry), maxLeftEntry, e)
            }
          // *re-balance is needed when the branch was merged
          // from the Br2 to Br3 or to the Leaf and the height is decreased
          case _: Branch2[K, V] if (!newRight.isInstanceOf[Branch2[K, V]]) =>
            left match {
              // the hole has a 2-node as a parent and a 3-node as a sibling:
              case b3: Branch3[K, V] =>
                new Branch2(
                  Branch2(b3.left, b3.e0, b3.mid),
                  b3.e1,
                  Branch2(b3.right, e, newRight)
                )
              // the the hole has a 2-node as a parent and a 2-node as a sibling:
              case _ =>
                val b2 = left.asInstanceOf[Branch2[K, V]]
                Branch3(b2.left, b2.e, b2.right, e, newRight)
            }
          case _ => Branch2(left, e, newRight)
        }
      } else {
        val newMidEntry = if (entry eq e) left.getMaxHashEntryOrNull else e
        // go downward:
        // swap the predecessor entry (max left entry) with the mid entry,
        // then proceed to remove the predecessor from the Left branch
        val newLeft = left.removeEntry(if (entry eq e) newMidEntry else entry)
        left match {
          // the Entry means that we are removing the left completely.
          case _: Entry[K, V] =>
            if (!right.closeToSplitAndRebalance)
              right.addOrGetEntry(newMidEntry)
            else {
              val rightMinEntry = right.getMinHashEntryOrNull
              val newRight = right.removeEntry(rightMinEntry)
              Branch2(newMidEntry, rightMinEntry, newRight)
            }
          // *re-balance is needed when the branch was merged
          // from Br2 to Br3 or to the leaf and the height decreased
          case _: Branch2[K, V] if (!newLeft.isInstanceOf[Branch2[K, V]]) =>
            right match {
              // the the hole has a 2-node as a parent and a 3-node as a sibling:
              case b3: Branch3[K, V] =>
                Branch2(
                  Branch2(newLeft, newMidEntry, b3.left),
                  b3.e0,
                  Branch2(b3.mid, b3.e1, b3.right)
                )
              // the the hole has a 2-node as a parent and a 2-node as a sibling.
              case _ => {
                val b2 = left.asInstanceOf[Branch2[K, V]]
                Branch3(newLeft, newMidEntry, b2.left, b2.e, b2.right)
              }
            }
          case _ =>
            Branch2(newLeft, newMidEntry, right)
        }
      }
    }
  }

  protected final case class Branch3[K, V](
      left: SMap[K, V],
      e0: Entry[K, V],
      mid: SMap[K, V],
      e1: Entry[K, V],
      right: SMap[K, V]
  ) extends SMap[K, V] {

    assert(e0.hash < e1.hash)

    override def closeToSplitAndRebalance = true

    override def size = left.size + e0.size + mid.size + e1.size + right.size
    override def getMinHashEntryOrNull = left.getMinHashEntryOrNull
    override def getMaxHashEntryOrNull = right.getMaxHashEntryOrNull

    override def getEntryOrNull(hash: Int) =
      if (hash > e1.hash) right.getEntryOrNull(hash)
      else if (hash < e0.hash) left.getEntryOrNull(hash)
      else if (hash == e0.hash) e0
      else if (hash == e1.hash) e1
      else mid.getEntryOrNull(hash)

    override def addOrGetEntry(entry: Entry[K, V]) = {
      val hash = entry.hash; val h0 = e0.hash; val h1 = e1.hash
      if (hash > h1)
        right.addOrGetEntry(entry) match {
          case e: Entry[K, V] => e
          case b2: Branch2[K, V] if (right.closeToSplitAndRebalance) =>
            Branch2(Branch2(left, e0, mid), e1, b2)
          case r =>
            Branch3(left, e0, mid, e1, r)
        }
      else if (hash < h0)
        left.addOrGetEntry(entry) match {
          case e: Entry[K, V] => e
          case b2: Branch2[K, V] if (left.closeToSplitAndRebalance) =>
            Branch2(b2, e0, Branch2(mid, e1, right))
          case l =>
            Branch3(l, e0, mid, e1, right)
        }
      else if (hash > h0 && hash < h1)
        mid.addOrGetEntry(entry) match {
          case e: Entry[K, V] => e
          case b2: Branch2[K, V] if (mid.closeToSplitAndRebalance) =>
            Branch2(
              Branch2(left, e0, b2.left),
              b2.e,
              new Branch2(b2.right, e1, right)
            )
          case m =>
            Branch3(left, e0, m, e1, right)
        }
      else if (hash == h0) e0
      else e1
    }

    override def replaceEntry(oldEntry: Entry[K, V], newEntry: Entry[K, V]) = {
      val h = oldEntry.hash; val h0 = e0.hash; val h1 = e1.hash
      if (h > h1)
        Branch3(left, e0, mid, e1, right.replaceEntry(oldEntry, newEntry))
      else if (h < h0)
        Branch3(left.replaceEntry(oldEntry, newEntry), e0, mid, e1, right)
      else if (h > h0 && h < h1)
        Branch3(left, e0, mid.replaceEntry(oldEntry, newEntry), e1, right)
      else if (h == h0) Branch3(left, newEntry, mid, e1, right)
      else Branch3(left, e0, mid, newEntry, right)
    }

    override def removeEntry(entry: Entry[K, V]) = {
      // case 1 downward:
      // swap the predecessor entry (max left entry) with the mid entry,
      // then proceed to remove the predecessor from the Left branch
      val h = entry.hash; val h0 = e0.hash; val h1 = e1.hash
      if (h <= h0) {
        val leftMaxEntry = if (h == h0) left.getMaxHashEntryOrNull else e0
        val newLeft = left.removeEntry(if (h == h0) leftMaxEntry else entry)
        left match {
          case _: Entry[K, V] =>
            mid match {
              case _: Leaf5PlusPlus[K, V] => {
                val midLeftEntry = mid.getMinHashEntryOrNull
                val newMid = mid.removeEntry(midLeftEntry)
                Branch3(leftMaxEntry, midLeftEntry, newMid, e1, right)
              }
              case _ =>
                Branch2(mid.addOrGetEntry(leftMaxEntry), e1, right)
            }
          // rebalance is needed because the branch was merged
          // from Br2 to Br3 or to Leaf and the height decreased
          case _: Branch2[K, V] if (!newLeft.isInstanceOf[Branch2[K, V]]) =>
            mid match {
              // the hole has a 3-node as a parent and a 3-node as a sibling.
              case b3: Branch3[K, V] =>
                Branch3(
                  Branch2(newLeft, leftMaxEntry, b3.left),
                  b3.e0,
                  Branch2(b3.mid, b3.e1, b3.right),
                  e1,
                  right
                )
              // the hole has a 3-node as a parent and a 2-node as a sibling.
              case _ => {
                val b2 = left.asInstanceOf[Branch2[K, V]]
                Branch2(
                  Branch3(newLeft, leftMaxEntry, b2.left, b2.e, b2.right),
                  e1,
                  right
                )
              }
            }
          case _ =>
            Branch3(newLeft, leftMaxEntry, mid, e1, right)
        }
      } else if (h <= h1) {
        val midMaxEntry = if (h == h1) mid.getMaxHashEntryOrNull else e1
        val newMid = mid.removeEntry(if (h == h1) midMaxEntry else entry)
        mid match {
          case _: Entry[K, V] =>
            right match {
              case _: Leaf5PlusPlus[K, V] => {
                val rightMinEntry = right.getMinHashEntryOrNull
                val newRight = right.removeEntry(rightMinEntry)
                Branch3(left, e0, midMaxEntry, rightMinEntry, newRight)
              }
              case _ =>
                Branch2(left, e0, right.addOrGetEntry(midMaxEntry))
            }
          case _: Branch2[K, V] if (!newMid.isInstanceOf[Branch2[K, V]]) =>
            right match {
              // the hole has a 3-node as a parent and a 3-node as a sibling.
              case b3: Branch3[K, V] =>
                Branch3(
                  left,
                  e0,
                  Branch2(newMid, midMaxEntry, b3.left),
                  b3.e0,
                  Branch2(b3.mid, b3.e1, b3.right)
                )
              // the hole has a 3-node as a parent and a 2-node as a sibling.
              case _ => {
                val b2 = left.asInstanceOf[Branch2[K, V]]
                Branch2(
                  left,
                  e0,
                  Branch3(newMid, midMaxEntry, b2.left, b2.e, b2.right)
                )
              }
            }
          case _ => Branch3(left, e0, newMid, midMaxEntry, right)
        }
      } else {
        val newRight = right.removeEntry(entry)
        right match {
          case _: Entry[K, V] =>
            mid match {
              case _: Leaf5PlusPlus[K, V] => {
                val midMaxEntry = mid.getMaxHashEntryOrNull
                val newMid = mid.removeEntry(midMaxEntry)
                Branch3(left, e0, newMid, midMaxEntry, e1)
              }
              case _ => Branch2(left, e0, mid.addOrGetEntry(e1))
            }
          case _: Branch2[K, V] if (!newRight.isInstanceOf[Branch2[K, V]]) =>
            mid match {
              case b3: Branch3[K, V] =>
                Branch3(
                  left,
                  e0,
                  Branch2(b3.left, b3.e0, b3.mid),
                  b3.e1,
                  Branch2(b3.right, e1, newRight)
                )
              case _ => {
                val b2 = left.asInstanceOf[Branch2[K, V]]
                Branch2(
                  left,
                  e0,
                  Branch3(b2.left, b2.e, b2.right, e1, newRight)
                )
              }
            }
          case _ =>
            Branch3(left, e0, mid, e1, newRight)
        }
      }
    }
  }
}
