import { fetchUsername, LOCAL_STORAGE_KEYS, Note } from './common.js';

const usernameSpan = document.getElementById('username');
usernameSpan.innerText = fetchUsername();

// Event listener that saves manual changes to the username in the local storage
usernameSpan.addEventListener('click', () => {
    const newUsername = prompt('Enter a new username:', '');
    if (newUsername === null) return; // User cancelled prompt
    localStorage.setItem(LOCAL_STORAGE_KEYS.USERNAME, newUsername);
    usernameSpan.innerText = newUsername;
});


/* Render existing notes */
const notesContainer = document.getElementById('notes');
Note.getAll().forEach(note => {
    notesContainer.appendChild(note.render());
});

/* Handler for creating a new note */
document.getElementById('create-note').addEventListener('click', () => 
{
    // Popup for the name
    const name = prompt('Enter the name of the new note:', '');
    if (name === null) return; // User cancelled prompt
    try {
        const note = new Note({ name: name });
        note.save();
        note.open();
    } 
    catch (error) {
        alert(error.message); // e.g. "A note's name cannot be empty"
    }
});
