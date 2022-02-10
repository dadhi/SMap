package example.map

trait SHashMap[K, V] {
  import SHashMap._
  def count: Int = 0

  def getMinHashEntryOrNull: Entry[K, V] = null;
  def getMaxHashEntryOrNull: Entry[K, V] = null;

  def getEntryOrNull(h: Int, k: Int): SHashMapEntry[K, V]

  def Update(newEntry: SHashMapEntry[K, V]): Entry[K, V]

  trait UpdaterOrKeeper[S] {
    def apply(
        state: S,
        oldEntry: SHashMapEntry[K, V],
        newEntry: SHashMapEntry[K, V]
    ): SHashMapEntry[K, V]
  }

  def UpdateOrKeep[S](state: S)(
      newEntry: SHashMapEntry[K, V],
      updateOrKeep: UpdaterOrKeeper[S]
  ): Entry[K, V]
}

case class SHashMapEntry[K, V](hash: Int, key: K, value: V)
    extends SHashMap.Entry[K, V] {

  override def count: Int = 1

  override def getEntryOrNull(h: Int, k: Int): SHashMapEntry[K, V] =
    if (hash == h && key == k) this else null

  override def Update(newEntry: SHashMapEntry[K, V]): SHashMap.Entry[K, V] =
    if (key == newEntry.key) newEntry
    else ??? //this.WithConflicting(newEntry);

  override def UpdateOrKeep[S](
      state: S
  )(
      newEntry: SHashMapEntry[K, V],
      updateOrKeep: UpdaterOrKeeper[S]
  ): SHashMap.Entry[K, V] =
    if (key != newEntry.key)
      ??? ///this.WithConflicting(newEntry);
    else if (updateOrKeep(state, this, newEntry) != this) newEntry
    else this
}

object SHashMap {

  trait Entry[K, V] extends SHashMap[K, V] {

    def hash: Int

    // internal sealed override Entry GetEntryOrNull(int hash) => hash == Hash ? this : null;

    // /// <summary>Lookup for the entry by Hash and Key</summary>
    // public abstract SHashMapEntry<K, V> GetEntryOrNull(int hash, K key);

    // /// <summary>Updating the entry with the new one</summary>
    // public abstract Entry Update(SHashMapEntry<K, V> newEntry);

    // /// <summary>Updating the entry with the new one using the `update` method</summary>
    // public abstract Entry UpdateOrKeep<S>(S state, SHashMapEntry<K, V> newEntry, UpdaterOrKeeper<S> updateOrKeep);

    // /// <inheritdoc />
    // public sealed override SHashMap<K, V> AddOrGetEntry(int hash, Entry entry) =>
    //     hash > Hash ? new Leaf2(this, entry) : hash < Hash ? new Leaf2(entry, this) : (SHashMap<K, V>)this;

    // /// <inheritdoc />
    // public sealed override SHashMap<K, V> ReplaceEntry(int hash, Entry oldEntry, Entry newEntry) =>
    //     this == oldEntry ? newEntry : oldEntry;

    // internal sealed override SHashMap<K, V> RemoveEntry(Entry removedEntry) =>
    //     this == removedEntry ? Empty : this;
  }
}
