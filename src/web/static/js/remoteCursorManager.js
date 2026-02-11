
export { RemoteCursorManager };

/**
 * Manages remote cursors for collaborative editing
 */
class RemoteCursorManager {
    constructor(textArea, overlay, charSize, padding, cols) {
        this.textArea = textArea;
        this.overlay = overlay;
        this.charSize = charSize;
        this.padding = padding;
        this.cols = cols;
        this.activeCursors = [];
        this.cursorLastActivity = new Map(); // username -> timestamp
        this.inactivityTimeoutMs = 10 * 60 * 1000;
        this.inactivityCheckFrequencyMs = 30 * 1000;
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
        
        // Create stretcher cursor for scrolling sync
        this.stretcher = this.#createStretcher();

        // Start the inactivity cleanup timer
        this.#startInactivityCleanup();
    }

    /**
     * Moves a remote cursor by username
     */
    moveCursorByName(username, charIndex) {
        const cursor = this.#createCursorIfNotExists(username);
        this.#moveCursor(cursor, charIndex);
        this.#updateActivity(username);
    }

    /**
     * Synchronizes all cursors starting from the specified index with an offset
     * Used when text is inserted or deleted to adjust cursor positions
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
     * Updates the stretcher cursor position to maintain scroll sync
     */
    updateStretcher() {
        this.#moveCursor(this.stretcher, this.textArea.value.length);
        this.stretcher.style.top = `${parseInt(this.stretcher.style.top) + this.padding}px`;
    }

    /**
     * Creates a hidden stretcher cursor that enables scrolling synchronization
     */
    #createStretcher() {
        const stretcher = document.createElement('div');
        stretcher.className = 'remote-cursor';
        stretcher.style.visibility = 'hidden';
        this.overlay.appendChild(stretcher);
        return stretcher;
    }

    /**
     * Starts a timer to periodically check for and remove inactive cursors
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
                this.#deleteCursor(username);
            }
        }, this.inactivityCheckFrequencyMs); // Check every minute
    }

    /**
     * Converts a character index to pixel coordinates
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
     * Updates the last activity timestamp for a user
     */
    #updateActivity(username) {
        this.cursorLastActivity.set(username, Date.now());
    }

    /**
     * Creates a cursor for the user if it doesn't already exist
     * Returns the existing cursor if it already exists
     */
    #createCursorIfNotExists(username) {
        const existingCursor = document.getElementById(`cursor-${username}`);
        if (existingCursor) {
            return existingCursor;
        }
        return this.#createCursor(username);
    }

    /**
     * Creates a new remote cursor for the specified user
     */
    #createCursor(username) {
        const cursor = document.createElement('div');
        cursor.id = `cursor-${username}`;
        cursor.className = 'remote-cursor';
        cursor.setAttribute('data-username', username);
        cursor.setAttribute('data-index', '0');
        cursor.style.backgroundColor = this.colors[this.activeCursors.length % this.colors.length];
        this.overlay.appendChild(cursor);
        this.activeCursors.push(cursor);
        this.#updateActivity(username);
        console.info(`Created cursor for user ${username}`)
        return cursor;
    }

    /**
     * Deletes the remote cursor for the specified user
     */
    #deleteCursor(username) {
        const cursor = document.getElementById(`cursor-${username}`);
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
     * Moves a cursor element to the specified character index
     */
    #moveCursor(cursor, charIndex) {
        if (charIndex < 0 || charIndex > this.textArea.value.length) { 
            console.warn(`Unable to move a remote cursor: expected a charIndex between 0 and ${this.textArea.value.length}, got ${charIndex} instead`);
            return;
        }
        
        cursor.setAttribute('data-index', charIndex);

        const { top, left } = this.#indexToCoordinates(charIndex);
        cursor.style.top = `${top}px`;
        cursor.style.left = `${left}px`;

        console.debug(`Moved ${cursor.getAttribute('data-username')} at index ${charIndex} (char before: '${this.textArea.value[charIndex - 1]}') -> {top: ${top}px, left: ${left}px}`);
    }

}
