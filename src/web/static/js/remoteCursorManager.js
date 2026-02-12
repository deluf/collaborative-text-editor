export { RemoteCursorManager };

/**
 * Manages remote cursors (the position of other users) for collaborative editing.
 * Handles the creation, movement, synchronization, and cleanup of cursors.
 */
class RemoteCursorManager {

    /**
     * Creates an instance of RemoteCursorManager.
     * * @param {HTMLTextAreaElement} textArea - The text area element where editing occurs.
     * @param {HTMLElement} overlay - The overlay container where cursor elements are appended.
     * @param {Object} charSize - The dimensions of a single character.
     * @param {number} charSize.width - The width of a character in pixels.
     * @param {number} charSize.height - The height of a character in pixels.
     * @param {number} padding - The padding inside the text area in pixels.
     * @param {number} cols - The number of columns (characters per line) for text wrapping calculations.
     */
    constructor(textArea, overlay, charSize, padding, cols) {
        this.textArea = textArea;
        this.overlay = overlay;
        this.charSize = charSize;
        this.padding = padding;
        this.cols = cols;

        /** @type {HTMLElement[]} List of active cursor DOM elements. */
        this.activeCursors = [];

        /** @type {Map<string, number>} Map of usernames to their last activity timestamp (ms). */
        this.cursorLastActivity = new Map();

        /** @type {number} Time in milliseconds before a cursor is considered inactive (10 mins). */
        this.inactivityTimeoutMs = 10 * 60 * 1000;

        /** @type {number} Frequency in milliseconds to check for inactive cursors (30 secs). */
        this.inactivityCheckFrequencyMs = 30 * 1000;

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
        this.#updateActivity(username);
    }

    /**
     * Synchronizes all cursors starting from the specified index with a given offset.
     * Used when text is inserted or deleted to shift other users' cursors accordingly.
     * * @param {number} index - The character index where the edit occurred.
     * @param {number} offset - The number of characters added (positive) or removed (negative).
     */
    synchronizeCursors(index, offset) {
        const start = index;
        const end = this.textArea.value.length - 1;
        
        for (const cursor of this.activeCursors) {
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
        for (const cursor of this.activeCursors) {
            this.#deleteCursor(cursor);
        }
    }    

    /**
     * Updates the stretcher cursor position to the end of the text.
     * This ensures the overlay height matches the textarea scroll height.
     */
    updateStretcher() {
        this.#moveCursor(this.stretcher, this.textArea.value.length);
        this.stretcher.style.top = `${parseInt(this.stretcher.style.top) + this.padding}px`;
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
            const usersToRemove = [];
            
            for (const [username, lastActivity] of this.cursorLastActivity.entries()) {
                if (now - lastActivity > this.inactivityTimeoutMs) {
                    usersToRemove.push(username);
                }
            }
            
            for (const username of usersToRemove) {
                console.info(`Removing inactive cursor for user: ${username}`);
                this.#deleteCursorByUsername(username);
            }
        }, this.inactivityCheckFrequencyMs); // Check every minute
    }

    /**
     * Converts a character index to pixel coordinates (top, left) relative to the text area.
     * Simulates text wrapping based on the column width.
     * * @private
     * @param {number} charIndex - The index of the character.
     * @returns {{top: number, left: number}} The coordinates in pixels.
     */
    #indexToCoordinates(charIndex) {
        const text = this.textArea.value;
        let row = 0;
        let col = 0;

        // Loop through text up to the specific index to simulate rendering
        for (let i = 0; i < charIndex; i++) {
            // If we hit a hard newline, reset col and increment row
            if (text[i] === '\n') {
                row++;
                col = 0;
                continue;
            }

            col++;
            if (col >= this.cols - 1) {
                row++;
                col = 0;
            }
        }

        const top = (row * this.charSize.height) + this.padding;
        const left = (col * this.charSize.width) + this.padding;

        return { top, left };
    }

    /**
     * Updates the last activity timestamp for a specific user.
     * * @private
     * @param {string} username - The username to update.
     */
    #updateActivity(username) {
        this.cursorLastActivity.set(username, Date.now());
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
        cursor.style.backgroundColor = this.colors[this.activeCursors.length % this.colors.length + this.colorOffset];
        this.overlay.appendChild(cursor);
        this.activeCursors.push(cursor);
        this.#updateActivity(username);
        console.info(`Created cursor for user ${username}`)
        return cursor;
    }

    /**
     * Deletes the remote cursor for the specified user.
     * * @private
     * @param {string} username - The username of the cursor to remove.
     */
    #deleteCursorByUsername(username) {
        const cursor = document.getElementById(`cursor-${username}`);
        this.#deleteCursor(cursor);
    }

    /**
     * Completely removes a remote cursor and all its data.
     * * @private
     * @param {HTMLElement} cursor - The cursor to remove.
     */
    #deleteCursor(cursor) {
        const index = this.activeCursors.indexOf(cursor);
        if (cursor) { 
            cursor.remove(); 
        }
        if (index > -1) { 
            this.activeCursors.splice(index, 1); 
        }
        this.cursorLastActivity.delete(username);
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
