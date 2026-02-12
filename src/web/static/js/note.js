import { fetchUsername, Note } from './common.js';

import { RemoteCursorManager } from "./remoteCursorManager.js";

import { SimpleCRDT } from "./simpleCRDT.js";
const CRDT = new SimpleCRDT();

import { Server, Edit, Sync } from "./server.js";

const USERNAME = fetchUsername();
const CONTAINER = document.getElementById('main');
const TEXT_AREA = document.getElementById('textarea');
const OVERLAY = document.getElementById('overlay');
const NOTE_NAME = document.getElementById('window-title');
const LAST_UPDATE_TIMESTAMP = document.getElementById('last-update-timestamp');
const LAST_UPDATE_USERNAME = document.getElementById('last-update-username');

function updateNoteStats(username=USERNAME) {
    LAST_UPDATE_TIMESTAMP.innerText = new Date().toLocaleString();
    LAST_UPDATE_USERNAME.innerText = username;
}

// Computes the width and height in pixels of a single character
function _computeCharSizePx() 
{
    const span = document.createElement('span');
    span.style.font = getComputedStyle(TEXT_AREA).font;
    span.style.visibility = 'hidden';
    span.textContent = 'M'; // Random char (the font is monospace) 
    document.body.appendChild(span);
    const width = span.getBoundingClientRect().width;
    const height = parseFloat(getComputedStyle(TEXT_AREA).lineHeight);
    document.body.removeChild(span);
    return { width, height };
}

const PADDING = 10;
const COLS = 80;
const ROWS = 24;
const CHAR_SIZE = _computeCharSizePx();

CONTAINER.style.width = `${COLS * CHAR_SIZE.width + PADDING * 2}px`;
CONTAINER.style.height = `${ROWS * CHAR_SIZE.height + PADDING * 2}px`;
TEXT_AREA.style.padding = `${PADDING}px`;
OVERLAY.style.padding = `${PADDING}px`;

const remoteCursorManager = new RemoteCursorManager(
    TEXT_AREA, OVERLAY, CHAR_SIZE, PADDING, COLS
);

// Scroll sync
TEXT_AREA.addEventListener('scroll', () => {
    OVERLAY.scrollTop = TEXT_AREA.scrollTop;
});

// Disables everything but single-char insert/delete in the text area
TEXT_AREA.addEventListener('beforeinput', (event) => {
    const { inputType, data } = event;
    
    // Strictly block every operation with multiple chars selected
    if (event.target.selectionEnd - event.target.selectionStart > 1) {
        event.preventDefault();
        return;
    }

    // Allow single-char deletions (backspace/delete)
    if (inputType.includes('delete')) { return; }

    // Allow single-char additions
    if (data && data.length === 1) { return; }

    // Allow newlines
    if (inputType === 'insertLineBreak' || inputType === 'insertParagraph') { return; }

    // Block everything else (pasting, dragging, multi-char autocomplete)
    event.preventDefault();
});

// Extra safety: Disable 'drop' to prevent text dragging
TEXT_AREA.addEventListener('drop', (event) => event.preventDefault());

let IS_MODIFYING_TEXT = false;

TEXT_AREA.addEventListener('input', (event) => {
    IS_MODIFYING_TEXT = true;

    // Move the STRETCHER cursor to the absolute bottom of the text area
    remoteCursorManager.updateStretcher();

    const { inputType, data } = event;
    const { selectionStart, value } = event.target;

    let index = selectionStart;
    if (inputType.startsWith('insert')) {
        index--;
        const newId = CRDT.getNewId(index);
        const edit = new Edit({
            username: USERNAME,
            action: "insert",
            id: newId,
            char: TEXT_AREA.value[index]
        });
        server.sendEdit(edit);
        CRDT.insert(newId);
        remoteCursorManager.synchronizeCursors(index, 1);
        updateNoteStats();
    }
  
    if (inputType.startsWith('delete')) {
        const edit = new Edit({
            username: USERNAME,
            action: "delete",
            id: CRDT.getIdFromIndex(index)
        });
        server.sendEdit(edit);
        CRDT.deleteFromIndex(index);
        remoteCursorManager.synchronizeCursors(index, -1);
        updateNoteStats();
    }
});

TEXT_AREA.addEventListener('selectionchange', () => {
    const start = TEXT_AREA.selectionStart;
    const end = TEXT_AREA.selectionEnd;
    // Ignore multi-char selections
    if (end !== start) { return; }
    // Ignore the selection change if it originated from a text edit
    if (IS_MODIFYING_TEXT) 
    {
        IS_MODIFYING_TEXT = false;
        return;
    }
    const edit = new Edit({
        username: USERNAME,
        action: "move",
        id: CRDT.getIdFromIndex(start)
    });
    server.sendEdit(edit);
});

console.info(`Loaded TEXT_AREA {
    rows: ${ROWS},
    cols: ${COLS},
    width: ${CONTAINER.style.width}, 
    height: ${CONTAINER.style.height}, 
    padding: ${PADDING},
    char_size: ${CHAR_SIZE.width} x ${CHAR_SIZE.height},
}`);


/**
 * callback to process messages coming from the server (websocket)
 * @param {Edit} edit 
 */
function processIncomingEditMessage(edit) 
{
    console.info("Received edit message: ", edit);
    
    if (edit.username === USERNAME) {
        console.warn("Ignoring mirrored update...");
        return;
    }

    let index;
    switch (edit.action) 
    {
        case 'insert':
            index = CRDT.insert(edit.id);
            console.debug(`${edit.username} inserted char '${edit.char}' at index ${index} (after char '${TEXT_AREA.value[index - 1]}')`);
            // TEXT_AREA.value = TEXT_AREA.value.slice(0, index) + edit.char + TEXT_AREA.value.slice(index);
            TEXT_AREA.setRangeText(edit.char, index, index, 'preserve');
            remoteCursorManager.synchronizeCursors(index, +1);
            remoteCursorManager.moveCursorByName(edit.username, index + 1);
            updateNoteStats(edit.username);
            break;
        case 'delete':
            index = CRDT.deleteFromId(edit.id);
            console.debug(`${edit.username} deleted char '${TEXT_AREA.value[index]}' at index ${index}`);
            // TEXT_AREA.value = TEXT_AREA.value.slice(0, index) + TEXT_AREA.value.slice(index + 1);
            TEXT_AREA.setRangeText('', index, index + 1, 'preserve');
            remoteCursorManager.synchronizeCursors(index, -1);
            remoteCursorManager.moveCursorByName(edit.username, index);
            updateNoteStats(edit.username);
            break;
        case 'move':
            index = CRDT.getIndexFromId(edit.id);
            console.debug(`${edit.username} moved at index ${index}`);
            remoteCursorManager.moveCursorByName(edit.username, index);
            break;
            // FIXME: c'è ancora qualche off-by-one nei cursori - provare la scrollbar
        default:
            console.warn(`Received unknown action '${edit.action}' from user '${edit.username}'`);
            return;
    }
}

/**
 * 
 * @param {Sync} sync 
 */
function processIncomingSyncMessage(sync) {
    console.info("Received sync message: ", sync);
    remoteCursorManager.deleteAll();
    TEXT_AREA.value = "";
    CRDT.clear();
    for (const delta of sync.data) {
        let index = CRDT.insert(delta.id);
        TEXT_AREA.setRangeText(delta.char, index, index, 'end');
    }
    updateNoteStats("<SERVER>");
}


/** 
 * Loads the specified note.
 * Creates it if the uuid is new.
 * Returns to the website index on error
 */
function loadNoteOrCreateIfNew(uuid, name) {
    const notes = Note.getAll();

    // The note already exists locally
    const savedNote = notes.find(note => note.uuid === uuid);
    if (savedNote) {
        NOTE_NAME.innerText = savedNote.name;

        // If the note is owned enable the share button
        if (savedNote.owned) {
            const shareNoteButton = document.getElementById('menu-bar-share');
            shareNoteButton.addEventListener('click', () => {
                const shareURL = savedNote.getShareURL();
                alert(`Share this URL (copied to your clipboard):\n${shareURL}`); 
                navigator.clipboard.writeText(shareURL);
            });
            shareNoteButton.className = 'menu-bar-enabled';
        }
    }

    // The note does not exist - import it
    else if (uuid && name) {
        const importedNote = new Note({
            uuid: uuid,
            name: name,
            owned: false
        });
        importedNote.saveToLocalStorage();
        NOTE_NAME.innerText = name;
    }

    // The note does not exist locally and no name is specified - abort
    else { window.location.href = '/'; }
}

// .../note?uuid=...&name=...
const params = new URLSearchParams(window.location.search);
const uuid = params.get('uuid');
const name = params.get('name');
loadNoteOrCreateIfNew(uuid, name);

const server = new Server(uuid, processIncomingEditMessage, processIncomingSyncMessage);


const deleteNoteButton = document.getElementById('menu-bar-delete');
deleteNoteButton.addEventListener('click', () => {
    Note.delete(uuid);
    window.location.href = '/';
});
deleteNoteButton.className = 'menu-bar-enabled';

const renameNoteButton = document.getElementById('menu-bar-rename');
renameNoteButton.addEventListener('click', () => {
    // Popup for the name
    const name = prompt('Enter the new name for this note:', '');
    if (name === null) return; // User cancelled prompt
    try {
        const newName = Note.rename(uuid, name);
        NOTE_NAME.innerText = newName;
    }
    catch (error) {
        alert(error.message); // e.g. "A note's name cannot be empty"
    }
});
renameNoteButton.className = 'menu-bar-enabled';


// FIXME: for debug - to remove
document.getElementById("clippy").addEventListener("click", () => {
    for (let i = 0; i < CRDT.ids.length; i++) {
        console.log(` index: ${i} ('${TEXT_AREA.value[i]}') --> id '${CRDT.ids[i]}' `)
    }
    console.log(CRDT.ids.toSorted());
});
