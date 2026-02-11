
import { generateKeyBetween } from 'https://esm.sh/jittered-fractional-indexing';

export { SimpleCRDT };

// Maps character index in the textarea -> fractional id (string)
class SimpleCRDT {
    constructor() {
        this.ids = []; 
    }

    /**
     * Computes the new fractional id for an insertion operation between index - 1 and index
     * @param {number} index 
     * @returns {string} id
     */
    getNewId(index) {
        const prevId = this.ids[index - 1];
        const nextId = this.ids[index];
        const newId = generateKeyBetween(prevId, nextId);
        return newId;
    }

    /**
     * Inserts the id in the map maintaining ascending order
     * @param {string} id 
     */
    insert(id) {
        const index = this.#binarySearch(id);
        this.ids.splice(index, 0, id);
        return index;
    }

    deleteFromId(id) {
        const index = this.#binarySearch(id);
        return this.deleteFromIndex(index);
    }

    deleteFromIndex(index) {
        this.ids.splice(index, 1);
        return index;
    }

    getIndexFromId(id) {
        return this.#binarySearch(id);
    }

    getIdFromIndex(index) {
        return this.ids[index];
    }

    /**
     * Finds the first index where `this.ids[index] >= id` in O(log(N)).
     * If the ID exists, returns its index.
     * If the ID does not exist, returns the index where it should be inserted
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
