import { fetchUsername, Note } from './common.js';

import { RemoteCursorManager } from "./remoteCursorManager.js";

import { SimpleCRDT } from "./simpleCRDT.js";
const CRDT = new SimpleCRDT();

import { Server, Edit } from "./server.js";


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
 * 
 * @param {Edit} edit 
 * @returns 
 */
function processIncomingRequest(edit) 
{
    if (edit.username === USERNAME) {
        console.warn("Ignored mirrored update: ", edit);
        return;
    }
    let index;
    switch (edit.action) 
    {
        case 'insert':
            index = CRDT.insert(edit.id);
            //console.info(`${username} inserted char '${character}' at index ${index} (after char '${TEXT_AREA.value[index - 1]}')`);
            TEXT_AREA.value = TEXT_AREA.value.slice(0, index) + edit.char + TEXT_AREA.value.slice(index);
            // FIXME: TEXT_AREA.setRangeText(edit.char, index, index, 'end');
            remoteCursorManager.synchronizeCursors(index, +1);
            remoteCursorManager.moveCursorByName(edit.username, index + 1);
            updateNoteStats(edit.username);
            break;
        case 'delete':
            index = CRDT.deleteFromId(edit.id);
            //console.info(`${username} deleted char '${TEXT_AREA.value[index]}' at index ${index}`);
            TEXT_AREA.value = TEXT_AREA.value.slice(0, index) + TEXT_AREA.value.slice(index + 1);
            remoteCursorManager.synchronizeCursors(index, -1);
            remoteCursorManager.moveCursorByName(edit.username, index);
            updateNoteStats(edit.username);
            break;
        case 'move':
            index = CRDT.getIndexFromId(edit.id);
            //console.info(`${username} moved at index ${index}`);
            remoteCursorManager.moveCursorByName(edit.username, index);
            break;
            // FIXME: action 'sync', data = [{id, char}, {id, char}, ...]
        default:
            console.warn(`Received unknown action '${edit.action}' from user '${edit.username}'`);
            return;
    }
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
        importedNote.save();
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

const server = new Server(uuid, processIncomingRequest);


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





// FIXME: demo 
// TEXT_AREA.value = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam nec finibus magna. Etiam eu ligula tincidunt, ornare odio eu, cursus ex. In erat nibh, blandit sed vestibulum eget, ultricies pellentesque lectus. Curabitur non felis risus. Aenean quis convallis sem. Mauris vel convallis ipsum. Aenean magna leo, facilisis quis quam sed, venenatis aliquam metus.\n\nInteger viverra sit amet sapien vitae bibendum. Maecenas vitae vehicula mi, sed venenatis odio. Morbi volutpat porttitor ultrices. Cras velit libero, gravida eget imperdiet eu, finibus sit amet arcu. Sed gravida convallis eros eget interdum. Vivamus ut purus in augue cursus semper. Donec tristique dui luctus, egestas enim sit amet, consequat justo.\n\nSuspendisse a ex convallis, fringilla felis sed, finibus lectus. Morbi vel sem sit amet leo volutpat ullamcorper eu tristique nisl. Proin at tortor viverra, tincidunt nulla vitae, porta erat. Nullam at dui ac ligula accumsan hendrerit at a nisl. Donec ex sapien, elementum sed lorem quis, fringilla egestas purus. Sed condimentum iaculis interdum. Praesent volutpat massa purus, sit amet euismod orci sagittis sit amet. Etiam bibendum ut sapien non dictum. Duis a tristique lacus. Mauris sed vestibulum lorem. Phasellus luctus libero at nunc cursus, vel facilisis dolor scelerisque. Mauris consectetur vitae enim a fermentum. Fusce vel suscipit quam, ac pharetra purus.";
document.getElementById("clippy").addEventListener("click", () => {
    for (let i = 0; i < CRDT.ids.length; i++) {
        console.log(` index: ${i} ('${TEXT_AREA.value[i]}') --> id '${CRDT.ids[i]}' `)
    }
    console.log(CRDT.ids.toSorted());
});
