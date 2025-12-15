const textarea = document.getElementById('input-area');
const overlay = document.getElementById('cursor-overlay');

// Computes the width and height in pixels of a single character
function getCharSize() {
    const span = document.createElement('span');
    span.style.font = getComputedStyle(textarea).font;
    span.style.visibility = 'hidden';
    span.textContent = 'M'; // Random char (the font is monospace) 
    document.body.appendChild(span);
    const width = span.getBoundingClientRect().width;
    const height = parseInt(getComputedStyle(textarea).lineHeight);
    document.body.removeChild(span);
    return { width, height };
}

const charSize = getCharSize();
const charsPerRow = Math.floor((800 - 20) / charSize.width);

console.log(`Char size: ${charSize}`);
console.log(`Chars per row: ${charsPerRow}`);

// Sync textarea and overlay vertical scrolls
textarea.addEventListener('scroll', () => {
    overlay.scrollTop = textarea.scrollTop;
});

// Renders a remote cursor
function updateRemoteCursor(userId, textIndex) {
    let cursor = document.getElementById(`cursor-${userId}`);
    
    // Create the cursor if it doesn't exist
    if (!cursor) {
        cursor = document.createElement('div');
        cursor.id = `cursor-${userId}`;
        cursor.className = 'remote-cursor';
        cursor.setAttribute('data-user', userId);
        overlay.appendChild(cursor);
    }

    console.log(`Drawing ${userId} at index ${textIndex}`);

    const rowIndex = Math.floor(textIndex / charsPerRow);
    const colIndex = textIndex % charsPerRow;

    console.log(`Result: rowIndex ${rowIndex}, colIndex ${colIndex}`);

    // Math: Grid coordinates -> Pixels
    // We add 10px padding because of the CSS padding
    const top = (rowIndex * charSize.height) + 10; 
    const left = (colIndex * charSize.width) + 10;

    cursor.style.top = `${top}px`;
    cursor.style.left = `${left}px`;
}

// DEMO - Another user moves its cursor around
setInterval(() => {
    updateRemoteCursor('UserB', Math.floor(Math.random() * textarea.value.length));
}, 1000);

