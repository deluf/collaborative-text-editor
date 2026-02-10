import { LOCAL_STORAGE_KEYS, Note } from './common.js';

/* Set the display name */
const displayNameInput = document.getElementById('display-name');
let currentDisplayName = localStorage.getItem(LOCAL_STORAGE_KEYS.DISPLAY_NAME);
// If the display name is already defined use that, otherwise generate it as a random integer 
if (!currentDisplayName) {
    currentDisplayName = String(Math.floor(Math.random() * 1_000_000_000));
    localStorage.setItem(LOCAL_STORAGE_KEYS.DISPLAY_NAME, currentDisplayName);
}
displayNameInput.value = currentDisplayName;
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
