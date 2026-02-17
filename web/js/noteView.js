import { NoteItem } from "./noteItem.js";
import { Database } from "./database.js";

export { NoteView };

/**
 * Handles the graphical user interface and event listeners for the open note
 */
class NoteView {

    // The maximum number of rendered characters of the "last updater" username
    static #LAST_UPDATER_MAX_LENGTH = 16;

    /** 
     * Creates an instance of NoteView
     * @param {NoteItem} openNote - The note object currently being viewed and edited
     * @param {function(number): void} onLocalInsert - Callback triggered when a character is inserted locally
     * @param {function(number): void} onLocalDelete - Callback triggered when a character is deleted locally
     * @param {function(number): void} onLocalMove - Callback triggered when the cursor position changes
     */
    constructor(openNote, onLocalInsert, onLocalDelete, onLocalMove) {
        this.GUI = {
            textArea: document.getElementById("textarea"),
            overlay: document.getElementById("overlay"),
            ghostTextArea: document.getElementById("ghost-textarea"),
            windowTitle: document.getElementById("window-title"),
            lastUpdateTimestamp: document.getElementById("last-update-timestamp"),
            lastUpdateUsername:  document.getElementById("last-update-username"),
            connectionStatus: document.getElementById("connection-status")
        }

        this.openNote = openNote;
        this.onLocalInsert = onLocalInsert;
        this.onLocalDelete = onLocalDelete;
        this.onLocalMove = onLocalMove;

        // Setup the window title
        this.GUI.windowTitle.innerText = this.openNote.name;

        this.#setupTextArea();
        this.#setupShareButton();
        this.#setupDeleteNoteButton();
        this.#setupRenameNoteButton();
    }

    /**
     * Updates the status bar with the latest available information
     * @param {string} username - The username of the person who just modified the note
     */
    updateStats(username) {
        // Ignore the (local) stats update if the connection is down
        if (this.GUI.connectionStatus.dataset.status === "offline") { return; }
        this.GUI.lastUpdateTimestamp.innerText = new Date().toLocaleString();
        if (username.length > NoteView.#LAST_UPDATER_MAX_LENGTH) {
            username = username.substring(0, NoteView.#LAST_UPDATER_MAX_LENGTH) + "…";
        }
        this.GUI.lastUpdateUsername.innerText = username;    
    }

    /**
     * Updates the connection status UI
     * @param {boolean} isOnline - True if connected, false otherwise
     */
    setConnectionStatus = (isOnline) => {
        const statusText = isOnline ? "online" : "offline";
        this.GUI.connectionStatus.innerText = statusText;
        this.GUI.connectionStatus.dataset.status = statusText;
    }

    /**
     * Sets up the event listeners for the text area. 
     * Restricts inputs to single-character insertions and deletions to support CRDT logic,
     *  and binds text modification and cursor movement to the respective local callbacks
     * @private
     */
    #setupTextArea() {
        // Disables everything but single-char insert/delete
        this.GUI.textArea.addEventListener("beforeinput", (event) => {
            const { inputType, data } = event;
            
            // Strictly block every operation with multiple chars selected
            if (event.target.selectionEnd - event.target.selectionStart > 1) {
                event.preventDefault();
                return;
            }

            // Allow single-char deletions (backspace only)
            if (inputType === "deleteContentBackward") { return; }

            // Allow single-char additions
            if (data && data.length === 1) { return; }

            // Allow newlines
            if (inputType === "insertLineBreak" || inputType === "insertParagraph") { return; }

            // Block everything else (e.g., pasting, dragging, multi-char autocomplete, ...)
            event.preventDefault();
        });

        // Extra safety: Disable "drop" to prevent text dragging
        this.GUI.textArea.addEventListener("drop", (event) => event.preventDefault());

        let IS_MODIFYING_TEXT = false;

        this.GUI.textArea.addEventListener("input", (event) => {
            IS_MODIFYING_TEXT = true;

            const { inputType } = event;
            const { selectionStart } = event.target;
            
            let index = selectionStart;
            if (inputType.startsWith("insert")) { this.onLocalInsert(index - 1); }
            else if (inputType.startsWith("delete")) { this.onLocalDelete(index); }
        });

        this.GUI.textArea.addEventListener("selectionchange", () => {
            const start = this.GUI.textArea.selectionStart;
            const end = this.GUI.textArea.selectionEnd;

            // Ignore multi-char selections
            if (end !== start) { return; }
            
            // Ignore the selection change if it originated from a text edit
            if (IS_MODIFYING_TEXT) { IS_MODIFYING_TEXT = false; return; }

            this.onLocalMove(start);
        });
    }

    /**
     * If the current user owns the note, enables a menu bar button that copies the note's share
     *  URL to the clipboard
     * @private
     */
    #setupShareButton() {
        if (!this.openNote.owned) { return; }
        const shareNoteButton = document.getElementById("menu-bar-share");
        shareNoteButton.addEventListener("click", () => {
            const shareURL = this.openNote.getShareURL();
            const shareURLprompt = `Share this URL: \n${shareURL}`;
            // Check if we are in a Secure Context with the Clipboard API available
            if (navigator.clipboard && typeof navigator.clipboard.writeText === 'function') {
                navigator.clipboard.writeText(shareURL)
                    .then(() => alert(shareURLprompt + "\n(The URL was copied to your clipboard)"))
                    .catch(() => alert(shareURLprompt));
            } 
            else { alert(shareURLprompt); } // Fallback
        });
        shareNoteButton.className = "menu-bar-enabled";
    }

    /**
     * Enables a menu bar button that allows the user to delete the current note from the database 
     *  and redirects to the home page
     * @private
     */
    #setupDeleteNoteButton() {
        const deleteNoteButton = document.getElementById("menu-bar-delete");
        deleteNoteButton.addEventListener("click", () => {
            Database.deleteNote(this.openNote.uuid);
            window.location.href = "/";
        });
        deleteNoteButton.className = "menu-bar-enabled";    
    }

    /**
     * Enables a menu bar button that prompts the user for a new name to assing to the current note
     * @private
     */
    #setupRenameNoteButton() {
        const renameNoteButton = document.getElementById("menu-bar-rename");
        renameNoteButton.addEventListener("click", () => {
            // Popup for the name
            let name = prompt("Enter the new name for this note:", "");
            if (name === null) return; // User cancelled prompt
            try {
                name = Database.renameNote(this.openNote.uuid, name);
                this.GUI.windowTitle.innerText = name;
            }
            catch (error) {
                alert(error.message); // e.g. "A note's name cannot be empty"
            }
        });
        renameNoteButton.className = "menu-bar-enabled";    
    }

}
