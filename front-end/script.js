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
    const height = parseInt(getComputedStyle(textArea).lineHeight);
    document.body.removeChild(span);
    return { width, height };
}

const { width: textAreaWidth, height: textAreaHeight } = container.getBoundingClientRect();
const textAreaPadding = parseInt(getComputedStyle(textArea).paddingTop); 
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

let activeUsers = [];

function createRemoteCursor(username) 
{
    cursor = document.createElement('div');
    cursor.id = `cursor-${username}`;
    cursor.className = 'remote-cursor';
    cursor.setAttribute('data-username', username);
    overlay.appendChild(cursor);
    cursor.style.backgroundColor = colors[activeUsers.length % colors.length];
    activeUsers.push(username);
}

function deleteRemoteCursor(username) 
{
    const cursor = document.getElementById(`cursor-${username}`);
    if (cursor) { cursor.remove(); }
    const index = activeUsers.indexOf(username);
    if (index > -1) { activeUsers.splice(index, 1); }
}

function updateRemoteCursor(username, charIndex) 
{
    let cursor = document.getElementById(`cursor-${username}`);
    if (!cursor) { console.error(`Error moving remote cursor: cursor-${username} does not exist`); }

    const rowIndex = Math.floor(charIndex / charsPerRow);
    const colIndex = charIndex % charsPerRow;


    const top = (rowIndex * charSize.height) + textAreaPadding; 
    const left = (colIndex * charSize.width) + textAreaPadding;

    console.log(`Moving ${username} at index ${charIndex} {row: ${rowIndex}, col: ${colIndex}} -> {top: ${top}px, left: ${left}px}`);
    
    cursor.style.top = `${top}px`;
    cursor.style.left = `${left}px`;
}

// TODO: DEMO - Remove in production
createRemoteCursor('User0');
createRemoteCursor('User1');
createRemoteCursor('User2');
setInterval(() => {
    updateRemoteCursor('User0', Math.floor(Math.random() * textArea.value.length));
    updateRemoteCursor('User1', Math.floor(Math.random() * textArea.value.length));
    updateRemoteCursor('User2', Math.floor(Math.random() * textArea.value.length));
}, 1000);

