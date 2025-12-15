const container = document.getElementById('editor-container');
const textArea = document.getElementById('input-area');
const overlay = document.getElementById('cursor-overlay');

// Sync textArea and overlay vertical scrolls
textArea.addEventListener('scroll', () => {
    overlay.scrollTop = textArea.scrollTop;
});

// Computes the width and height in pixels of a single character
function getCharSize() 
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
const charSize = getCharSize();
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

    //const textBefore = textArea.value.slice(0, charIndex);
    //const newlineCount = textBefore.split('\n').length - 1;

    //const replacementSpaces = ' '.repeat(charsPerRow);
    //const flattenedTextArea = textBefore.value.replace(/\n/g, replacementSpaces);

    const rowIndex = Math.floor(charIndex / charsPerRow);
    const colIndex = charIndex % charsPerRow;
    const top = (rowIndex * charSize.height) + textAreaPadding; 
    const left = (colIndex * charSize.width) + textAreaPadding;

    cursor.style.top = `${top}px`;
    cursor.style.left = `${left}px`;

    console.log(`Moved ${username} at index ${charIndex} {row: ${rowIndex}, col: ${colIndex}} -> {top: ${top}px, left: ${left}px}`);
}

function decrementCursors()
{
    const delta = charSize.width / 2;
    for (const cursor of activeCursors)
    {
        const left = parseFloat(cursor.style.left) - charSize.width;
        const top = parseFloat(cursor.style.top) - charSize.height;
       
        // Same row
        if (left > textAreaPadding - delta) 
        { 
            cursor.style.left = `${left}px`;
            continue; 
        }

        // Back to the first character
        if (top < textAreaPadding - delta)
        {
            cursor.style.left = `${textAreaPadding}px`;
            cursor.style.top = `${textAreaPadding}px`;
            continue;
        }

        // Upper row
        cursor.style.left = `${charSize.width * charsPerRow}px`;
        cursor.style.top = `${top}px`;
    }
}

function incrementCursors()
{
    const delta = charSize.width / 2;
    for (const cursor of activeCursors)
    {
        const left = parseFloat(cursor.style.left) + charSize.width;

        // Same row
        console.log(left, charsPerRow, charsPerRow * charSize.width, charsPerRow * charSize.width + delta, left < (charsPerRow * charSize.width + delta)); 
        if (left < (charsPerRow * charSize.width + delta)) 
        { 
            cursor.style.left = `${left}px`;
            continue; 
        }

        // Lower row
        cursor.style.left = `${textAreaPadding}px`;
        cursor.style.top = `${parseFloat(cursor.style.top) + charSize.height}px`;
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

updateRemoteCursor('User0', Math.floor(Math.random() * textArea.value.length));
updateRemoteCursor('User1', Math.floor(Math.random() * textArea.value.length));
updateRemoteCursor('User2', Math.floor(Math.random() * textArea.value.length));

setInterval(() => {
    return;
    updateRemoteCursor('User0', Math.floor(Math.random() * textArea.value.length));
    updateRemoteCursor('User1', Math.floor(Math.random() * textArea.value.length));
    updateRemoteCursor('User2', Math.floor(Math.random() * textArea.value.length));
}, 500);

