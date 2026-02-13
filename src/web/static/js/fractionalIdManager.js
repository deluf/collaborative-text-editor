"use strict";

import { generateKeyBetween } from 'https://esm.sh/jittered-fractional-indexing';

export { FractionalIdManager };

/**
 * A simple Conflict-free Replicated Data Type (CRDT) helper class.
 * It maps a linear, gap-free, numeric index (e.g., the index of a character in a string) to a 
 *  unique fractional ID.
 * Fractional IDs are provided by https://github.com/nathanhleung/jittered-fractional-indexing 
 *  (implemented as strings for infinite precision with a random suffix for concurrency) and are
 *  maintained in a sorted array
 */
class FractionalIdManager {

    /** 
     * @private Fractional ID assigned to the very last index + 1.
     * 
     * Imagine a distributed text editor application where this class is used to map the index of
     *  characters in a text to unique fractional IDs:
     * 
     * When a user moves the cursor, the software finds the fractional ID of the character following
     *  the cursor, and sends the update to the server (e.g., A|BC => Sends fractionalId(B)).
     * 
     * Now, imagine a user moves its cursor *after* the very last character to write some text at
     *  the end of the document, in order to signal to other clients where the the cursor moved, 
     *  the update must carry a fractional ID, but there is no ID assigned to anything *after* the
     *  very last character! (e.g., ABC| => Sends fractionalId(???)).
     * 
     * The EOF_MARKER soves exactly this problem: (e.g., ABC| => Send fractionalId(EOF_MARKER)).
     */
    static #EOF_MARKER = "<<EOF>>";

    /**
     * Initializes the FractionalIdManager instance with an empty list of IDs
     */
    constructor() {
        /**
         * An ordered (ascending) array of fractional IDs
         * @type {string[]}
         */
        this.ids = []; 
    }

    /**
     * Computes a new fractional ID for an insertion operation.
     * The new ID is generated to fall lexicographically between the ID at `index - 1`
     *  and the ID at `index`
     * @param {number} index - The numeric insertion index (0-based)
     * @returns {string} The new unique fractional ID
     */
    getNewId(index) {
        let prevId = this.getIdFromIndex(index - 1);
        let nextId = this.getIdFromIndex(index);

        // The jittered-fractional-indexing library expects undefined IDs
        //  when they don't exist (e.g., to generate the very first ID or
        //  to generate the ID at the start/end of the text)
        prevId = prevId === FractionalIdManager.#EOF_MARKER ? undefined : prevId;
        nextId = nextId === FractionalIdManager.#EOF_MARKER ? undefined : nextId;
        
        return generateKeyBetween(prevId, nextId);;
    }

    /**
     * Inserts a fractional ID into the internal map while maintaining the order.
     * If the ID already exists it is not inserted twice
     * @param {string} id - The fractional ID to insert
     * @returns {number} The numeric index where the ID was inserted, -1 if it already existed
     */
    insert(id) {
        const { index, found } = this.#binarySearch(id);
        if (found) { return -1; }
        this.ids.splice(index, 0, id);
        return index;
    }

    /**
     * Deletes a specific fractional ID from the internal map
     * @param {string} id - The fractional ID to delete
     * @returns {number} The numeric index from which the ID was removed, -1 if the ID did not exist
     */
    deleteFromId(id) {
        const { index, found } = this.#binarySearch(id);
        if (!found) { return -1; }
        this.deleteFromIndex(index); 
        return index;
    }

    /**
     * Deletes the fractional ID located at the specified numeric index.
     * If the index is out of bounds nothing is deleted
     * @param {number} index - The numeric index to delete from
     */
    deleteFromIndex(index) {
        if (index < 0 || index >= this.ids.length) { return; }
        this.ids.splice(index, 1);
    }

    /**
     * Retrieves the current numeric index for a given fractional ID.
     * If the ID does not exists or it is the EOF marker, it returns
     *  the length of the text (i.e., the last valid index + 1)
     * @param {string} id - The fractional ID to locate
     * @returns {number} The index of the ID in the list
     */
    getIndexFromId(id) {
        if (id === FractionalIdManager.#EOF_MARKER) { return this.ids.length; }
        const { index, found } = this.#binarySearch(id);
        if (!found) { return this.ids.length; }
        return index;
    }

    /**
     * Retrieves the fractional ID located at a specific numeric index.
     * If the index is out of bounds returns the EOF marker
     * @param {number} index - The numeric index
     * @returns {string} The fractional ID at that index (or the EOF marker)
     */
    getIdFromIndex(index) {
        if (index < 0 || index >= this.ids.length) { return FractionalIdManager.#EOF_MARKER; }
        return this.ids[index];
    }

    /**
     * Clears all fractional IDs from the internal map, resetting the state
     */
    clear() {
        this.ids = [];
    }

    /**
     * Finds the character index corresponding to a give ID.
     * If the ID does not exists, returns the index where it should be inserted.
     * @private
     * @param {string} id - The ID to search for
     * @returns {{index: number, found: boolean}} 
     * index: The location of the ID, if present, otherwise, the index where it should be inserted.
     * found: True if the ID actually exists at that index.
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

        // Check if the item at the 'low' index is actually the one we wanted
        const found = low < this.ids.length && this.ids[low] === id;    
        return { index: low, found };
    }

}
