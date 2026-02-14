"use strict";

export { NoteItem };

/**
 * Represents a single note item containing metadata and rendering logic.
 * Handles serialization, DOM creation for the dashboard, and navigation
 */
class NoteItem {

    /**
     * Creates a new NoteItem instance
     * @param {Object} data - The initialization data
     * @param {string} [data.uuid] - The unique identifier for the note. If not provided, one will be generated
     * @param {string} data.name - The display name of the note
     * @param {boolean} [data.owned=true] - Indicates if the current user owns this note (affects icon styling)
     * @throws {Error} Will throw an error if the name is missing or empty
     */
    constructor({ uuid, name, owned = true }) {
        if (!name || name.trim() === "") {
            throw new Error("A note's name cannot be empty");
        }
        this.uuid = uuid || this.#generateSafeUUID();
        this.name = name.trim();
        this.owned = owned;
    }

    /**
     * Generates a UUIDv4 safely, falling back if crypto.randomUUID is unavailable
     * @private
     */
    #generateSafeUUID() {
        if (typeof self.crypto !== 'undefined' && self.crypto.randomUUID) {
            return self.crypto.randomUUID();
        }
        
        // Fallback for insecure contexts (file:// or http://)
        console.warn("crypto.randomUUID unavailable - using a fallback UUID generator");
        return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
            const r = Math.random() * 16 | 0;
            const v = c === 'x' ? r : (r & 0x3 | 0x8);
            return v.toString(16);
        });
    }

    /**
     * Generates the DOM element for the dashboard list.
     * Creates an anchor tag containing an icon (based on ownership) and the note name
     * @returns {HTMLAnchorElement} The complete dashboard element for this note.
     */
    render() {    
        const link = document.createElement("a");
        link.href = `/note?uuid=${this.uuid}`;
        
        const icon = document.createElement("div");
        icon.className = this.owned ? "owned-note-icon" : "shared-note-icon";

        const name = document.createElement("div");
        name.textContent = this.name;

        link.appendChild(icon);
        link.appendChild(name);
        
        return link;
    }

    /**
     * Constructs the shareable URL for the note
     * @returns {string} The absolute URL for sharing the note
     */
    getShareURL() {
        const origin = window.location.origin; 
        const safeName = encodeURIComponent(this.name);
        return `${origin}/note?uuid=${this.uuid}&name=${safeName}`;
    }

    /**
     * Navigates the browser window to the specific note page
     * @returns {never}
     */
    open() {
        window.location.href = `/note?uuid=${this.uuid}`;
    }
    
}
