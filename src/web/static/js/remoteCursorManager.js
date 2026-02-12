export { RemoteCursorManager };

/**
 * Manages remote cursors (the position of other users) for collaborative editing.
 * Handles the creation, movement, synchronization, and cleanup of cursors.
 */
class RemoteCursorManager {

    /** @private Time in milliseconds before a cursor is considered inactive (5 mins). */
    static #INACTIVITY_TIMEOUT_MS = 5 * 30 * 1000;

    /** @private Frequency in milliseconds to check for inactive cursors. */
    static #INACTIVITY_CHECK_FREQUENCY_MS = RemoteCursorManager.#INACTIVITY_TIMEOUT_MS / 10;

    /**
     * Creates an instance of RemoteCursorManager.
     * * @param {HTMLTextAreaElement} textArea - The text area element where editing occurs.
     * @param {HTMLElement} overlay - The overlay container where cursor elements are appended.
     * @param {HTMLElement} mirror - The textarea mirror used to calculate caret coordinates.
     * @param {number} padding - The padding inside the text area in pixels.
     */
    constructor(textArea, overlay, mirror, padding) {
        this.textArea = textArea;
        this.overlay = overlay;
        this.mirror = mirror;
        this.padding = padding;

        /** @type {Map<HTMLElement, number>} Maps each (active) cursor to their last activity timestamp (ms) */
        this.activeCursors = new Map();

        /** @type {string[]} List of colors assigned to cursors */
        this.colors = [
            '#FFADAD', 
            '#CAFFBF',
            '#A0C4FF',
            '#F4E1D2', 
            '#D4F0F0',
            '#BDB2FF',
            '#FFD6A5',
            '#9BF6FF',
            '#FDFFB6',
            '#FFC6FF'
        ];

        // Offset that randomizes color assignment to cursors
        this.colorOffset = Math.floor(Math.random() * this.colors.length);
        
        // Create stretcher cursor for scrolling sync
        this.stretcher = this.#createStretcher();

        // Start the inactivity cleanup timer
        this.#startInactivityCleanup();
    }

    /**
     * Moves a remote cursor identified by a username to a specific character index.
     * If the cursor does not exist, it is created.
     * * @param {string} username - The unique identifier for the user.
     * @param {number} charIndex - The index in the text string where the cursor should be placed.
     */
    moveCursorByName(username, charIndex) {
        const cursor = this.#createCursorIfNotExists(username);
        this.#moveCursor(cursor, charIndex);
        this.#updateActivity(cursor);
    }

    /**
     * Synchronizes all cursors starting from the specified index with a given offset.
     * Used when text is inserted or deleted to shift other users' cursors accordingly.
     * * @param {number} index - The character index where the edit occurred.
     * @param {number} offset - The number of characters added (positive) or removed (negative). Must be either -1 or +1!
     */
    synchronizeCursors(index, offset) {
        return;
        const start = index;
        const end = this.textArea.value.length - 1;
        
        for (const cursor of this.activeCursors.keys()) {
            const currentIndex = parseInt(cursor.getAttribute('data-index') || '0');
            if (currentIndex >= start && currentIndex <= end) {
                const newIndex = Math.min(this.textArea.value.length, currentIndex + offset);
                this.#moveCursor(cursor, newIndex);
            }
        }
    }

    /**
     * Removes all active remote cursors from the overlay and clears all activity data.
     * Use this to reset the collaborative state or clean up when disconnecting.
     */
    deleteAll() {
        for (const cursor of this.activeCursors.keys()) {
            this.#deleteCursor(cursor);
        }
    }    

    /**
     * Updates the stretcher cursor position to the end of the text.
     * This ensures the overlay height matches the textarea scroll height.
     */
    updateStretcher() {
        this.#moveCursor(this.stretcher, this.textArea.value.length);
        const currentTop = parseInt(this.stretcher.style.top) || 0;
        this.stretcher.style.top = `${currentTop + this.padding}px`;
    }

    /**
     * Creates a hidden stretcher cursor that enables scrolling synchronization.
     * * @private
     * @returns {HTMLElement} The created stretcher element.
     */
    #createStretcher() {
        const stretcher = document.createElement('div');
        stretcher.className = 'remote-cursor';
        stretcher.style.visibility = 'hidden';
        this.overlay.appendChild(stretcher);
        return stretcher;
    }

    /**
     * Starts a periodic timer to check for and remove cursors that have been inactive
     *  for longer than the defined timeout.
     * * @private
     */
    #startInactivityCleanup() {
        // Check every minute for inactive cursors
        this.cleanupInterval = setInterval(() => {
            const now = Date.now();
            for (const [cursor, lastActivity] of this.activeCursors.entries()) {
                if (now - lastActivity > RemoteCursorManager.#INACTIVITY_TIMEOUT_MS) {
                    console.info(`Removing inactive cursor for user: ${cursor.getAttribute("data-username")}`);
                    this.#deleteCursor(cursor);
                }
            }
        }, RemoteCursorManager.#INACTIVITY_CHECK_FREQUENCY_MS); // Check every minute
    }

    /**
     * Converts a character index to pixel coordinates (top, left) relative to the text area.
     * Simulates text wrapping based on the column width.
     * * @private
     * @param {number} charIndex - The index of the character.
     * @returns {{top: number, left: number}} The coordinates in pixels.
     */
    #indexToCoordinates(charIndex) {
        // FIXME:

        // Copy text up to the cursor position
        // We replace the actual char at the index with a span to measure it
        const text = this.textArea.value.substring(0, charIndex);
        
        // Set the text content of the mirror
        this.mirror.textContent = text;

        // Append a span to mark the cursor position
        const span = document.createElement('span');
        span.textContent = '#'; // The character doesn't matter, we just need position
        this.mirror.appendChild(span);

        // Read coordinates directly from the span
        // Note: We subtract scrollTop if you want coordinates relative to the viewport, 
        //  but since your cursors are in a scrolling Overlay that syncs with the textarea, 
        //  absolute offsetTop is likely what you want.
        return {
            top: span.offsetTop,
            left: span.offsetLeft
        };
    }

    /**
     * Updates the last activity timestamp for a specific cursor.
     * * @private
     * @param {HTMLElement} cursor - The cursor DOM element.
     */
    #updateActivity(cursor) {
        this.activeCursors.set(cursor, Date.now());
    }

    /**
     * Retrieves an existing cursor or creates a new one if it doesn't exist.
     * * @private
     * @param {string} username - The username associated with the cursor.
     * @returns {HTMLElement} The cursor DOM element.
     */
    #createCursorIfNotExists(username) {
        const existingCursor = document.getElementById(`cursor-${username}`);
        if (existingCursor) {
            return existingCursor;
        }
        return this.#createCursor(username);
    }

    /**
     * Creates a new remote cursor DOM element for the specified user.
     * Assigns a color from the predefined palette based on the number of active cursors.
     * * @private
     * @param {string} username - The username for the new cursor.
     * @returns {HTMLElement} The newly created cursor element.
     */
    #createCursor(username) {
        const cursor = document.createElement('div');
        cursor.id = `cursor-${username}`;
        cursor.className = 'remote-cursor';
        cursor.setAttribute('data-username', username);
        cursor.setAttribute('data-index', '0');
        cursor.style.backgroundColor = this.colors[(this.activeCursors.size + this.colorOffset) % this.colors.length];
        this.overlay.appendChild(cursor);
        this.#updateActivity(cursor);
        console.info(`Created cursor for user ${username}`)
        return cursor;
    }

    /**
     * Completely removes a remote cursor and all its data.
     * * @private
     * @param {HTMLElement} cursor - The cursor to remove.
     */
    #deleteCursor(cursor) {
        this.activeCursors.delete(cursor);
        cursor.remove();
    }

    /**
     * Moves a specific cursor element to the physical coordinates corresponding
     * to the provided character index.
     * * @private
     * @param {HTMLElement} cursor - The cursor DOM element to move.
     * @param {number} charIndex - The target character index.
     */
    #moveCursor(cursor, charIndex) {
        if (charIndex < 0 || charIndex > this.textArea.value.length) { 
            console.warn(`Unable to move a remote cursor: expected a charIndex between 0
                and ${this.textArea.value.length}, got ${charIndex} instead`);
            return;
        }
        
        cursor.setAttribute('data-index', charIndex);

        const { top, left } = this.#indexToCoordinates(charIndex);
        cursor.style.top = `${top}px`;
        cursor.style.left = `${left}px`;

        console.debug(`Moved ${cursor.getAttribute('data-username')} at index ${charIndex} 
            (char before: '${this.textArea.value[charIndex - 1]}') -> {top: ${top}px, left: ${left}px}`);
    }

}
