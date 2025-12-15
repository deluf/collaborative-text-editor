const CONTAINER = document.getElementById('editor-container');
const TEXT_AREA = document.getElementById('textarea');
const OVERLAY = document.getElementById('overlay');

// Sync TEXT_AREA and OVERLAY vertical scrolls
// FIXME:
TEXT_AREA.addEventListener('scroll', () => {
    OVERLAY.scrollTop = TEXT_AREA.scrollTop;
});

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

console.log(`Loaded TEXT_AREA {
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
  '#D4F0F0',
  '#BDB2FF',
  '#FFD6A5',
  '#9BF6FF',
  '#FDFFB6',
  '#FFC6FF',
  '#F4E1D2' 
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
        if (col >= COLS) {
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

function updateRemoteCursor(username, charIndex) 
{
    const cursor = document.getElementById(`cursor-${username}`);
    if (!cursor) { console.warn(`Unable to move a remote cursor: cursor-${username} does not exist`); }
    if (charIndex < 0 || charIndex > TEXT_AREA.value.length) { 
        console.warn(`Unable to move a remote cursor: expected a charIndex between 0 and ${TEXT_AREA.value.length}, got ${charIndex} instead`); }
    
    cursor.setAttribute('data-index', charIndex);

    const { top, left } = _indexToCoordinates(charIndex);
    cursor.style.top = `${top}px`;
    cursor.style.left = `${left}px`;

    console.log(`Moved ${username} at index ${charIndex} -> {top: ${top}px, left: ${left}px}`);
}

// Increments all the cursors above a certain character
function incrementCursors(thresholdIndex) {
    for (const cursor of ACTIVE_CURSORS) {
        let currentIndex = parseInt(cursor.getAttribute('data-index') || '0');
        if (currentIndex >= thresholdIndex) {
            const username = cursor.getAttribute('data-username');
            const newIndex = Math.min(TEXT_AREA.value.length, currentIndex + 1);
            updateRemoteCursor(username, newIndex);
        }
    }
}

// Decrements all the cursors above a certain character
function decrementCursors(thresholdIndex) {
    for (const cursor of ACTIVE_CURSORS) {
        let currentIndex = parseInt(cursor.getAttribute('data-index') || '0');
        if (currentIndex > thresholdIndex) {
            const username = cursor.getAttribute('data-username');
            const newIndex = Math.max(0, currentIndex - 1);
            updateRemoteCursor(username, newIndex);
        }
    }
}

function processIncomingRequest(username, action, character, index) 
{
    const currentContent = TEXT_AREA.value;
    let newContent = currentContent;

    switch (action) 
    {
        case 'CREATE':
            newContent = currentContent.slice(0, index) + character + currentContent.slice(index);
            incrementCursors();
            break;
        case 'UPDATE':
            newContent = currentContent.slice(0, index) + character + currentContent.slice(index + 1);
            break;
        case 'DELETE':
            newContent = currentContent.slice(0, index) + currentContent.slice(index + 1);
            decrementCursors();
            break;
        default:
            console.warn(`Received unknown action '${action}' from user '${username}'`);
            return;
    }

    console.log(`${username} ${action} character '${character}' at indxe ${index}`);
    TEXT_AREA.value = newContent;
}

// TODO: DEMO - Remove in production
createRemoteCursor('User0');
createRemoteCursor('User1');
createRemoteCursor('User2');

function randomCursors() 
{
    updateRemoteCursor('User0', Math.floor(Math.random() * TEXT_AREA.value.length));
    updateRemoteCursor('User1', Math.floor(Math.random() * TEXT_AREA.value.length));
    updateRemoteCursor('User2', Math.floor(Math.random() * TEXT_AREA.value.length));
}

setInterval(() => {
    return;
    updateRemoteCursor('User0', Math.floor(Math.random() * TEXT_AREA.value.length));
    updateRemoteCursor('User1', Math.floor(Math.random() * TEXT_AREA.value.length));
    updateRemoteCursor('User2', Math.floor(Math.random() * TEXT_AREA.value.length));
}, 5000);

