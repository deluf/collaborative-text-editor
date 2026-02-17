"use strict";

export { RemoteCursorManager };

/**
 * Manages remote cursors (the position of other users) for collaborative editing.
 * Handles the creation, movement, synchronization, and cleanup of cursors
 */
class RemoteCursorManager {

    /**
     * Creates an instance of RemoteCursorManager
     * * @param {HTMLTextAreaElement} textArea - The text area element where editing occurs
     * @param {HTMLElement} overlay - The overlay container where cursor elements are appended
     * @param {HTMLElement} ghostTextArea - The div used to calculate cursor coordinates
     */
    constructor(textArea, overlay, ghostTextArea) {
        this.textArea = textArea;
        this.overlay = overlay;
        this.ghostTextArea = ghostTextArea;

        this.padding = parseInt(window.getComputedStyle(this.textArea).padding);
        
        this.activeCursors = [];

        /** @type {string[]} List of colors assigned to cursors */
        this.colors = [
            "#FFADAD", 
            "#CAFFBF",
            "#A0C4FF",
            "#F4E1D2", 
            "#D4F0F0",
            "#BDB2FF",
            "#FFD6A5",
            "#9BF6FF",
            "#E1E575",
            "#FFC6FF"
        ];

        // Offset that randomizes color assignment to cursors
        this.colorOffset = Math.floor(Math.random() * this.colors.length);

        // Integer used to cycle through colors, assigning a different one at each connected user
        this.colorCount = 0;
        
        /**
         * In order to implement height and scroll synchronization between the textarea and the overlay
         *  (i.e., when the user scrolls the text the cursors move vertically), two conditions must occur:
         *  1) Their height must be the same
         *  2) Their scroll offset must be the same
         * 
         * The second problem is simply fixed by adding a scroll event listener:
         */
        this.textArea.addEventListener("scroll", () => {
            this.overlay.scrollTop = this.textArea.scrollTop;
        });
        /**
         * To fix the first problem, we create a dummy cursor that always points to the very last character.
         * This way, the height of the two container are exactly the same
         */
        this.stretcher = this.#createStretcherCursor();
    }

    /**
     * Moves a remote cursor identified by a username to a specific character index.
     * If the cursor does not exist, it is created.
     * If the index is out of bounds, moves it to the top left corner
     * @param {string} username - The unique identifier for the user
     * @param {number} index - The index in the text string where the cursor should be placed
     */
    moveCursorByName(username, index) {
        // index = this.textArea.value.length is allowed! (moves the cursor to the end of the text)
        if (index < 0 || index > this.textArea.value.length) { 
            console.warn(`Tried to move the cursor of user ${username} to index ${index} (expected 
                at most ${this.textArea.value.length}) - moving it to the top-left corner`);
            index = 0; 
        }

        const cursor = this.#createCursorIfNotExists(username);
        this.#moveCursor(cursor, index);
    }

    /**
     * Completely removes a remote cursor and all its data
     * @param {string} username - The unique identifier for the user
     */
    deleteCursorByName(username) {
        const cursor = document.getElementById(`cursor-${username}`);
        if (!cursor) { return; }
        this.activeCursors = this.activeCursors.filter(c => c != cursor);
        cursor.remove();
    }

    /**
     * Removes all active remote cursors from the overlay
     */
    clear() {
        this.activeCursors.forEach(cursor => cursor.remove());
        this.activeCursors = []
    }

    /**
     * Synchronizes the height of the overlay with the one of the textarea.
     * Must be called each time the textarea is modified to ensure scrolling synch
     *  between the cursors and the text
     */
    overlayHeightSync() {
        this.#moveCursor(this.stretcher, this.textArea.value.length);
        // Adds the textarea padding to the position of the cursor to fully stretch the overlay
        const currentTop = parseInt(this.stretcher.style.top) || 0;
        this.stretcher.style.top = `${currentTop + this.padding}px`;
    }

    /**
     * Creates a hidden stretcher cursor that enables scrolling synchronization
     * @private
     * @returns {HTMLElement} The created stretcher element
     */
    #createStretcherCursor() {
        const stretcher = document.createElement("div");
        stretcher.className = "remote-cursor";
        stretcher.style.visibility = "hidden";
        this.overlay.appendChild(stretcher);
        return stretcher;
    }

    /**
     * Retrieves an existing cursor or creates a new one if it doesn't exist
     * @private
     * @param {string} username - The username associated with the cursor
     * @returns {HTMLElement} The cursor DOM element
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
     * * @private
     * @param {string} username - The username for the new cursor
     * @returns {HTMLElement} The newly created cursor element
     */
    #createCursor(username) {
        const cursor = document.createElement("div");
        cursor.id = `cursor-${username}`;
        cursor.className = "remote-cursor";
        cursor.setAttribute("data-username", username);
        cursor.setAttribute("data-index", "0");
        cursor.style.backgroundColor = this.colors[
            (this.colorCount + this.colorOffset) % this.colors.length
        ];
        this.overlay.appendChild(cursor);
        this.activeCursors.push(cursor);
        this.colorCount++;
        console.info(`Created cursor for user ${username}`)
        return cursor;
    }

    /**
     * Moves a specific cursor element to the physical coordinates corresponding
     *  to the provided character index
     * @private
     * @param {HTMLElement} cursor - The cursor DOM element to move
     * @param {number} index - The target character index
     */
    #moveCursor(cursor, index) {
        cursor.setAttribute("data-index", index);
        const { top, left } = this.#indexToCoordinates(index);
        cursor.style.top = `${top}px`;
        cursor.style.left = `${left}px`;
    }

    /**
     * Converts a character index to pixel coordinates (top, left) relative to the text area
     * @private
     * @param {number} index - The index of the character
     * @returns {{top: number, left: number}} The coordinates in pixels
     */
    #indexToCoordinates(index) {
        // Copy text up to the cursor position to the ghost textarea
        const text = this.textArea.value.substring(0, index);
        this.ghostTextArea.textContent = text;
        // This also removes any child node of the ghost textarea
        // (there is no need to remove the created <span>s manually)

        // Append a span to the ghost textarea - this marks the cursor position
        const span = document.createElement("span");
        span.textContent = "#"; // The character doesn't matter, we just need the position
        this.ghostTextArea.appendChild(span);

        // Read the coordinates directly from the span
        return {
            top: span.offsetTop,
            left: span.offsetLeft
        };
    }

}
