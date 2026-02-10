import { generateKeyBetween } from 'https://esm.sh/jittered-fractional-indexing';
import { Note } from './common.js';

const CONTAINER = document.getElementById('main');
const TEXT_AREA = document.getElementById('textarea');
const OVERLAY = document.getElementById('overlay');
const NOTE_NAME = document.getElementById('window-title'); // FIXME:
const LAST_UPDATE_TIMESTAMP = document.getElementById('last-update-timestamp'); // FIXME:
const LAST_UPDATE_USERNAME = document.getElementById('last-update-uername'); // FIXME:

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

// Dummy cursor that stretches the overlay thus enabling cursors scrolling sync 
const STRETCHER = document.createElement('div');
STRETCHER.className = 'remote-cursor';
STRETCHER.style.visibility = 'hidden';
OVERLAY.appendChild(STRETCHER);

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
    moveRemoteCursor(STRETCHER, TEXT_AREA.value.length);
    STRETCHER.style.top = `${parseInt(STRETCHER.style.top) + PADDING}px`;
 
    const { inputType, data } = event;
    const { selectionStart, value } = event.target;

    let index = selectionStart;
    if (inputType.startsWith('insert')) {
        index--;
        // TODO: Send the INSERT request
        synchronizeCursors(index, +1);
        console.info(`Sent { username: 'User0', action: 'INSERT', character: '${TEXT_AREA.value[index]}', index: ${index} }`);
    }
  
    if (inputType.startsWith('delete')) {
        // TODO: Send the DELETE request
        synchronizeCursors(index, -1);
        console.info(`Sent { username: 'User0', action: 'DELETE', character: null, index: ${index} }`);
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
    // TODO: Send a MOVE request
    console.info(`Sent { username: 'User0', action: 'MOVE', character: null, index: ${start} }`);
});

console.info(`Loaded TEXT_AREA {
    rows: ${ROWS},
    cols: ${COLS},
    width: ${CONTAINER.style.width}, 
    height: ${CONTAINER.style.height}, 
    padding: ${PADDING},
    char_size: ${CHAR_SIZE.width} x ${CHAR_SIZE.height},
}`);

const COLORS = [
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

let ACTIVE_CURSORS = [];

function _indexToCoordinates(charIndex) 
{
    const text = TEXT_AREA.value;
    let row = 0;
    let col = 0;

    // Loop through text up to the specific index to simulate rendering
    for (let i = 0; i < charIndex; i++) 
    {
        // If we hit a hard newline, reset col and increment row
        if (text[i] === '\n') {
            row++;
            col = 0;
            continue;
        } 

        col++;
        if (col >= COLS - 1) {
            row++;
            col = 0;
        }
    }

    const top = (row * CHAR_SIZE.height) + PADDING;
    const left = (col * CHAR_SIZE.width) + PADDING;

    return { top, left };
}

function createRemoteCursor(username) 
{
    const cursor = document.createElement('div');
    cursor.id = `cursor-${username}`;
    cursor.className = 'remote-cursor';
    cursor.setAttribute('data-username', username);
    cursor.setAttribute('data-index', '0');
    cursor.style.backgroundColor = COLORS[ACTIVE_CURSORS.length % COLORS.length];
    OVERLAY.appendChild(cursor);
    ACTIVE_CURSORS.push(cursor);
}

function deleteRemoteCursor(username) 
{
    const cursor = document.getElementById(`cursor-${username}`);
    const index = ACTIVE_CURSORS.indexOf(cursor);
    if (cursor) { cursor.remove(); }
    if (index > -1) { ACTIVE_CURSORS.splice(index, 1); }
}

function moveRemoteCursorByName(username, charIndex)
{
    const cursor = document.getElementById(`cursor-${username}`);
    if (!cursor) { 
        console.warn(`Unable to move a remote cursor: cursor-${username} does not exist`); 
        return;
    }
    moveRemoteCursor(cursor, charIndex);
}

function moveRemoteCursor(cursor, charIndex) 
{
    if (charIndex < 0 || charIndex > TEXT_AREA.value.length) { 
        console.warn(`Unable to move a remote cursor: 
            expected a charIndex between 0 and ${TEXT_AREA.value.length}, got ${charIndex} instead`); 
        return;
    }
    
    cursor.setAttribute('data-index', charIndex);

    const { top, left } = _indexToCoordinates(charIndex);
    cursor.style.top = `${top}px`;
    cursor.style.left = `${left}px`;

    console.debug(`Moved ${cursor.getAttribute('data-username')} at index ${charIndex} (char before: '${TEXT_AREA.value[charIndex - 1]}') -> {top: ${top}px, left: ${left}px}`);
}

// Synchronizes all cursors starting from 'index' to their data-index attribute plus the specified offset
function synchronizeCursors(index, offset) 
{
    const start = index;
    const end = TEXT_AREA.value.length - 1;
    for (const cursor of ACTIVE_CURSORS) {
        const currentIndex = parseInt(cursor.getAttribute('data-index') || '0');
        if (currentIndex >= start && currentIndex <= end) {
            const newIndex = Math.min(TEXT_AREA.value.length, currentIndex + offset);
            moveRemoteCursor(cursor, newIndex);
        }
    }
}


function processIncomingRequest(username, action, character, id) 
{
    let index;
    switch (action) 
    {
        case 'INSERT':
            index = CRDT.insert(id);
            console.info(`${username} inserted char '${character}' at index ${index} (after char '${TEXT_AREA.value[index - 1]}')`);
            TEXT_AREA.value = TEXT_AREA.value.slice(0, index) + character + TEXT_AREA.value.slice(index);
            synchronizeCursors(index, +1);
            moveRemoteCursorByName(username, index + 1);
            break;
        case 'DELETE':
            index = CRDT.delete(id);
            console.info(`${username} deleted char '${TEXT_AREA.value[index]}' at index ${index}`);
            TEXT_AREA.value = TEXT_AREA.value.slice(0, index) + TEXT_AREA.value.slice(index + 1);
            synchronizeCursors(index, -1);
            moveRemoteCursorByName(username, index);
            break;
        case 'MOVE':
            index = CRDT.getIndex(id);
            console.info(`${username} moved at index ${index}`);
            moveRemoteCursorByName(username, index);
            break;
        default:
            console.warn(`Received unknown action '${action}' from user '${username}'`);
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
                alert(`Share this URL:\n${savedNote.getShareURL()}`); 
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
loadNoteOrCreateIfNew(uuid, name)






// DEMO - Simulates incoming network requests 

createRemoteCursor('User1');
createRemoteCursor('User2');
createRemoteCursor('User3');

async function demo()
{
    await new Promise(r => setTimeout(r, 1000));
    const users = ['User1', 'User2', 'User3'];
    for (const user of users) {
        let randomIndex = Math.floor(Math.random() * TEXT_AREA.value.length);
        processIncomingRequest(user, 'MOVE', null, CRDT.ids[randomIndex]);
        /*
        await new Promise(r => setTimeout(r, 500));
        for (const character of user) {
            processIncomingRequest(user, 'INSERT', character, randomIndex);
            randomIndex++;
            await new Promise(r => setTimeout(r, 500));
        }
        
        randomIndex--;
        for (character of user) {
            processIncomingRequest(user, 'DELETE', character, randomIndex);
            randomIndex--;
            await new Promise(r => setTimeout(r, 500));
        }
        */
    }
}


// Maps character index in the textarea -> fractional id (string)
class SimpleCRDT {
    constructor() {
        this.ids = []; 
    }

    /**
     * Computes the new fractional id for an insertion operation between index and index + 1
     * @param {number} index 
     * @returns {string} id
     */
    getNewId(index) {
        const prevId = this.ids[index];
        const nextId = this.ids[index + 1];
        return generateKeyBetween(prevId, nextId);
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

    delete(id) {
        const index = this.#binarySearch(id);
        this.ids.splice(index, 1);
        return index;
    }

    getIndex(id) {
        return this.#binarySearch(id);
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


let CRDT = new SimpleCRDT();

TEXT_AREA.value = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam nec finibus magna. Etiam eu ligula tincidunt, ornare odio eu, cursus ex.\nIn erat nibh,\n\nblandit sed vestibulum eget, ultricies pellentesque lectus";
for (let i = 0; i < TEXT_AREA.value.length; i++) {
    const id = CRDT.getNewId(i);
    CRDT.insert(id);
}
//TEXT_AREA.value = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam nec finibus magna. Etiam eu ligula tincidunt, ornare odio eu, cursus ex. In erat nibh, blandit sed vestibulum eget, ultricies pellentesque lectus. Curabitur non felis risus. Aenean quis convallis sem. Mauris vel convallis ipsum. Aenean magna leo, facilisis quis quam sed, venenatis aliquam metus.\n\nInteger viverra sit amet sapien vitae bibendum. Maecenas vitae vehicula mi, sed venenatis odio. Morbi volutpat porttitor ultrices. Cras velit libero, gravida eget imperdiet eu, finibus sit amet arcu. Sed gravida convallis eros eget interdum. Vivamus ut purus in augue cursus semper. Donec tristique dui luctus, egestas enim sit amet, consequat justo.\n\nSuspendisse a ex convallis, fringilla felis sed, finibus lectus. Morbi vel sem sit amet leo volutpat ullamcorper eu tristique nisl. Proin at tortor viverra, tincidunt nulla vitae, porta erat. Nullam at dui ac ligula accumsan hendrerit at a nisl. Donec ex sapien, elementum sed lorem quis, fringilla egestas purus. Sed condimentum iaculis interdum. Praesent volutpat massa purus, sit amet euismod orci sagittis sit amet. Etiam bibendum ut sapien non dictum. Duis a tristique lacus. Mauris sed vestibulum lorem. Phasellus luctus libero at nunc cursus, vel facilisis dolor scelerisque. Mauris consectetur vitae enim a fermentum. Fusce vel suscipit quam, ac pharetra purus.";

console.info(CRDT.ids)

demo();

