package example.map

trait SHashMap[K, +V] {

  val count: Int = 0

  private def getMinHashEntryOrNull: Entry = null;
  private def getMaxHashEntryOrNull: Entry = null;

  private def getEntryOrNull(hash: Int): Entry = null;

  type UpdaterOrKeeper[S] = ((S, KVEntry, KVEntry) => KVEntry) {
    def apply(state: S, oldEntry: KVEntry, newEntry: KVEntry): KVEntry
  }

  def UpdateOrKeep[S](
      state: S,
      newEntry: KVEntry,
      updateOrKeep: UpdaterOrKeeper[S]
  ): Entry = null;

  trait Entry extends SHashMap[K, V] {

    //   val hash: Int

    //   def getEntryOrNull(hash: Int, key: Int): KVEntry

    //   // internal sealed override Entry GetEntryOrNull(int hash) => hash == Hash ? this : null;

    //   // /// <summary>Lookup for the entry by Hash and Key</summary>
    //   // public abstract SHashMapEntry<K, V> GetEntryOrNull(int hash, K key);

    //   def Update(newEntry: KVEntry): Entry

    //   // /// <summary>Updating the entry with the new one using the `update` method</summary>
    //   // public abstract Entry UpdateOrKeep<S>(S state, SHashMapEntry<K, V> newEntry, UpdaterOrKeeper<S> updateOrKeep);

    //   // /// <inheritdoc />
    //   // public sealed override SHashMap<K, V> AddOrGetEntry(int hash, Entry entry) =>
    //   //     hash > Hash ? new Leaf2(this, entry) : hash < Hash ? new Leaf2(entry, this) : (SHashMap<K, V>)this;

    //   // /// <inheritdoc />
    //   // public sealed override SHashMap<K, V> ReplaceEntry(int hash, Entry oldEntry, Entry newEntry) =>
    //   //     this == oldEntry ? newEntry : oldEntry;

    //   // internal sealed override SHashMap<K, V> RemoveEntry(Entry removedEntry) =>
    //   //     this == removedEntry ? Empty : this;
  }

  abstract class KVEntry(hash: Int, key: K, value: V) extends Entry {

    //   override val count: Int = 1

    //   override def getEntryOrNull(h: Int, k: Int): KVEntry =
    //     if (hash == h && key == k) e else null

    //   override def Update(newEntry: KVEntry): Entry =
    //     if (key == newEntry.key) newEntry
    //     else ??? //this.WithConflicting(newEntry);

    //   override def UpdateOrKeep[S](
    //       state: S,
    //       newEntry: Entry,
    //       updateOrKeep: UpdaterOrKeeper[S]
    //   ): Entry =
    //     if (key != newEntry.key)
    //       ??? ///this.WithConflicting(newEntry);
    //     else if (updateOrKeep(state, this, newEntry) != this) newEntry
    //     else this
  }
}

object SHashMap {
  case object Empty extends SHashMap[Any, Nothing]
  def empty[K, V]: SHashMap[K, V] = Empty.asInstanceOf[SHashMap[K, V]]
}
