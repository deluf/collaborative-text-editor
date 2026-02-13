import { NoteItem } from "./noteItem.js";
import { Database } from "./database.js";

export { NoteView };

class NoteView {

    /** 
     * @param {NoteItem} openNote - ...
     */
    constructor(username, openNote, onLocalInsert, onLocalDelete, onLocalMove) {
        this.GUI = {
            textArea: document.getElementById('textarea'),
            overlay: document.getElementById('overlay'),
            ghostTextArea: document.getElementById('ghost-textarea'),
            windowTitle: document.getElementById('window-title'),
            lastUpdateTimestamp: document.getElementById('last-update-timestamp'),
            lastUpdateUsername:  document.getElementById('last-update-username'),
        }

        this.username = username;
        this.openNote = openNote;
        this.onLocalInsert = onLocalInsert;
        this.onLocalDelete = onLocalDelete;
        this.onLocalMove = onLocalMove;

        this.GUI.windowTitle.innerText = this.openNote.name;

        this.#setupTextArea();
        this.#setupShareButton();
        this.#setupDeleteNoteButton();
        this.#setupRenameNoteButton();
    }

    updateStats(username=this.username) {
        this.GUI.lastUpdateTimestamp.innerText = new Date().toLocaleString();
        this.GUI.lastUpdateUsername.innerText = username;    
    }

    #setupShareButton() {
        if (!this.openNote.owned) { return; }
        const shareNoteButton = document.getElementById('menu-bar-share');
        shareNoteButton.addEventListener('click', () => {
            const shareURL = this.openNote.getShareURL();
            navigator.clipboard.writeText(shareURL);
            alert(`Share this URL (copied to your clipboard):\n${shareURL}`); 
        });
        shareNoteButton.className = 'menu-bar-enabled';
    }

    #setupDeleteNoteButton() {
        const deleteNoteButton = document.getElementById('menu-bar-delete');
        deleteNoteButton.addEventListener('click', () => {
            Database.deleteNote(this.openNote.uuid);
            window.location.href = '/';
        });
        deleteNoteButton.className = 'menu-bar-enabled';    
    }

    #setupRenameNoteButton() {
        const renameNoteButton = document.getElementById('menu-bar-rename');
        renameNoteButton.addEventListener('click', () => {
            // Popup for the name
            let name = prompt('Enter the new name for this note:', '');
            if (name === null) return; // User cancelled prompt
            try {
                name = Database.renameNote(this.openNote.uuid, name);
                this.GUI.windowTitle.innerText = name;
            }
            catch (error) {
                alert(error.message); // e.g. "A note's name cannot be empty"
            }
        });
        renameNoteButton.className = 'menu-bar-enabled';    
    }

    #setupTextArea() {
        // Disables everything but single-char insert/delete
        this.GUI.textArea.addEventListener('beforeinput', (event) => {
            const { inputType, data } = event;
            
            // Strictly block every operation with multiple chars selected
            if (event.target.selectionEnd - event.target.selectionStart > 1) {
                event.preventDefault();
                return;
            }

            // Allow single-char deletions (backspace/delete)
            if (inputType === 'deleteContentBackward') { return; }

            // Allow single-char additions
            if (data && data.length === 1) { return; }

            // Allow newlines
            if (inputType === 'insertLineBreak' || inputType === 'insertParagraph') { return; }

            // Block everything else (pasting, dragging, multi-char autocomplete)
            event.preventDefault();
        });

        // Extra safety: Disable 'drop' to prevent text dragging
        this.GUI.textArea.addEventListener('drop', (event) => event.preventDefault());

        let IS_MODIFYING_TEXT = false;

        this.GUI.textArea.addEventListener('input', (event) => {
            IS_MODIFYING_TEXT = true;

            const { inputType } = event;
            const { selectionStart } = event.target;
            
            let index = selectionStart;
            let edit;
            if (inputType.startsWith('insert')) {
                index--;
                this.onLocalInsert(index);
            }
        
            if (inputType.startsWith('delete')) {
                this.onLocalDelete(index);
            }

            this.updateStats();
        });

        this.GUI.textArea.addEventListener('selectionchange', () => {
            const start = this.GUI.textArea.selectionStart;
            const end = this.GUI.textArea.selectionEnd;
            // Ignore multi-char selections
            if (end !== start) { return; }
            // Ignore the selection change if it originated from a text edit
            if (IS_MODIFYING_TEXT) 
            {
                IS_MODIFYING_TEXT = false;
                return;
            }
            this.onLocalMove(start);
        });
    }

}
