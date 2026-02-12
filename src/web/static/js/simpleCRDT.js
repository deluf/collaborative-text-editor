import { generateKeyBetween } from 'https://esm.sh/jittered-fractional-indexing';

export { SimpleCRDT };

/**
 * A simple Conflict-free Replicated Data Type (CRDT) helper class.
 * It maps a numeric character index (e.g., the index of a character in a string) to a unique fractional ID.
 * Fractional IDs are provided by https://github.com/nathanhleung/jittered-fractional-indexing (implemented 
 *  as strings for infinite precision with a random suffix for concurrency) and are maintained in a sorted array.
 */
class SimpleCRDT {

    /** @private Fractional ID assigned to the end of the note */
    static #EOF_MARKER = "<<EOF>>";

    /**
     * Initializes the SimpleCRDT instance with an empty list of IDs.
     */
    constructor() {
        /**
         * An ordered array of fractional IDs.
         * @type {string[]}
         */
        this.ids = []; 
    }

    /**
     * Computes a new fractional ID for an insertion operation.
     * The new ID is generated to fall lexicographically between the ID at `index - 1`
     *  and the ID at `index`.
     * * @param {number} index - The numeric insertion index (0-based).
     * @returns {string} The new unique fractional ID.
     */
    getNewId(index) {
        const prevId = this.getIdFromIndex(index - 1);
        let nextId = this.getIdFromIndex(index);
        nextId = nextId === SimpleCRDT.#EOF_MARKER ? undefined : nextId;
        const newId = generateKeyBetween(prevId, nextId);
        return newId;
    }

    /**
     * Inserts a fractional ID into the internal map while maintaining ascending order.
     * * @param {string} id - The fractional ID to insert.
     * @returns {number} The numeric index where the ID was inserted.
     */
    insert(id) {
        const index = this.#binarySearch(id);
        this.ids.splice(index, 0, id);
        return index;
    }

    /**
     * Deletes a specific fractional ID from the internal map.
     * * @param {string} id - The fractional ID to delete.
     * @returns {number} The numeric index from which the ID was removed.
     */
    deleteFromId(id) {
        const index = this.#binarySearch(id);
        this.deleteFromIndex(index);
        return index;
    }

    /**
     * Deletes the fractional ID located at the specified numeric index.
     * * @param {number} index - The numeric index to delete from.
     */
    deleteFromIndex(index) {
        this.ids.splice(index, 1);
    }

    /**
     * Retrieves the current numeric index for a given fractional ID.
     * * @param {string} id - The fractional ID to locate.
     * @returns {number} The index of the ID in the list.
     */
    getIndexFromId(id) {
        if (id === SimpleCRDT.#EOF_MARKER) { return this.ids.length; }
        return this.#binarySearch(id);
    }

    /**
     * Retrieves the fractional ID located at a specific numeric index.
     * * @param {number} index - The numeric index.
     * @returns {string} The fractional ID at that index.
     */
    getIdFromIndex(index) {
        if (index === this.ids.length) { return SimpleCRDT.#EOF_MARKER; }
        return this.ids[index];
    }

    /**
     * Clears all fractional IDs from the internal map, resetting the state.
     */
    clear() {
        this.ids = [];
    }

    /**
     * Finds the first index where `this.ids[index] >= id` using binary search.
     * * @private
     * @param {string} id - The ID to search for.
     * @returns {number} If the ID exists, returns its index. 
     * If the ID does not exist, returns the index where it should be inserted.
     */
    #binarySearch(id) {
        let low = 0;
        let high = this.ids.length;

        while (low < high) {
            const mid = Math.floor((low + high) / 2);
            if (this.ids[mid] < id) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        return low;
    }
}
