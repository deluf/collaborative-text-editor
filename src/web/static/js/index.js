import { fetchDisplayName, LOCAL_STORAGE_KEYS, Note } from './common.js';

const displayNameInput = document.getElementById('display-name');
displayNameInput.value = fetchDisplayName();

// Event listener that saves manual changes to the display name in the local storage
displayNameInput.addEventListener('input', (event) => {
    localStorage.setItem(LOCAL_STORAGE_KEYS.DISPLAY_NAME, event.target.value);
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
