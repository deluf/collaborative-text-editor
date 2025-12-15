const container = document.getElementById('editor-container');
const textArea = document.getElementById('input-area');
const overlay = document.getElementById('cursor-overlay');

// Sync textArea and overlay vertical scrolls
// FIXME:
textArea.addEventListener('scroll', () => {
    overlay.scrollTop = textArea.scrollTop;
});

// Computes the width and height in pixels of a single character
function _computeCharSize() 
{
    const span = document.createElement('span');
    span.style.font = getComputedStyle(textArea).font;
    span.style.visibility = 'hidden';
    span.textContent = 'M'; // Random char (the font is monospace) 
    document.body.appendChild(span);
    const width = span.getBoundingClientRect().width;
    const height = parseFloat(getComputedStyle(textArea).lineHeight);
    document.body.removeChild(span);
    return { width, height };
}

const { width: textAreaWidth, height: textAreaHeight } = container.getBoundingClientRect();
const textAreaPadding = parseFloat(getComputedStyle(textArea).paddingTop); 
const charSize = _computeCharSize();
const charsPerRow = Math.floor((textAreaWidth - 2 * textAreaPadding) / charSize.width);

console.log(`Loaded textArea {
    width: ${textAreaWidth}, 
    height: ${textAreaHeight}, 
    padding: ${textAreaPadding},
    charSize: ${charSize.width} x ${charSize.height},
    charsPerRow: ${charsPerRow}
}`);

const colors = [
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

let activeCursors = [];

function _indexToCoordinates(charIndex) 
{
    const text = textArea.value;
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
        if (col >= charsPerRow) {
            row++;
            col = 0;
        }
    }

    const top = (row * charSize.height) + textAreaPadding;
    const left = (col * charSize.width) + textAreaPadding;

    return { top, left };
}

function createRemoteCursor(username) 
{
    cursor = document.createElement('div');
    cursor.id = `cursor-${username}`;
    cursor.className = 'remote-cursor';
    cursor.setAttribute('data-username', username);
    overlay.appendChild(cursor);
    cursor.style.backgroundColor = colors[activeCursors.length % colors.length];
    activeCursors.push(cursor);
}

function deleteRemoteCursor(username) 
{
    const cursor = document.getElementById(`cursor-${username}`);
    const index = activeCursors.indexOf(cursor);
    if (cursor) { cursor.remove(); }
    if (index > -1) { activeCursors.splice(index, 1); }
}

function updateRemoteCursor(username, charIndex) 
{
    const cursor = document.getElementById(`cursor-${username}`);
    if (!cursor) { console.warn(`Unable to move a remote cursor: cursor-${username} does not exist`); }
    if (charIndex < 0 || charIndex > textArea.value.length) { 
        console.warn(`Unable to move a remote cursor: expected a charIndex between 0 and ${textArea.value.length}, got ${charIndex} instead`); }
    
    cursor.setAttribute('data-index', charIndex);

    const { top, left } = _indexToCoordinates(charIndex);
    cursor.style.top = `${top}px`;
    cursor.style.left = `${left}px`;

    console.log(`Moved ${username} at index ${charIndex} -> {top: ${top}px, left: ${left}px}`);
}

// Increments all the cursors above a certain character
function incrementCursors(thresholdIndex) {
    for (const cursor of activeCursors) {
        let currentIndex = parseInt(cursor.getAttribute('data-index') || '0');
        if (currentIndex >= thresholdIndex) {
            const username = cursor.getAttribute('data-username');
            const newIndex = Math.min(textArea.value.length, currentIndex + 1);
            updateRemoteCursor(username, newIndex);
        }
    }
}

// Decrements all the cursors above a certain character
function decrementCursors(thresholdIndex) {
    for (const cursor of activeCursors) {
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
    const currentContent = textArea.value;
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

    myTextArea.value = newContent;

}

// TODO: DEMO - Remove in production
createRemoteCursor('User0');
createRemoteCursor('User1');
createRemoteCursor('User2');

function randomCursors() 
{
    updateRemoteCursor('User0', Math.floor(Math.random() * textArea.value.length));
    updateRemoteCursor('User1', Math.floor(Math.random() * textArea.value.length));
    updateRemoteCursor('User2', Math.floor(Math.random() * textArea.value.length));
}

setInterval(() => {
    return;
    updateRemoteCursor('User0', Math.floor(Math.random() * textArea.value.length));
    updateRemoteCursor('User1', Math.floor(Math.random() * textArea.value.length));
    updateRemoteCursor('User2', Math.floor(Math.random() * textArea.value.length));
}, 5000);

