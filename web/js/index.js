"use strict";

import { NoteItem } from "./noteItem.js";
import { Database } from "./database.js";

/* Handle the username */ 
const usernameSpan = document.getElementById("username");
usernameSpan.innerText = Database.getUsername();
usernameSpan.addEventListener("click", () => {
    const newUsername = prompt("Enter your new username:", "");
    if (newUsername === null) { return; } // User cancelled prompt
    usernameSpan.innerText = newUsername;
    Database.setUsername(newUsername);
});

/* Render existing notes */
const notesContainer = document.getElementById("notes");
const notesFragment = document.createDocumentFragment(); // PERF: use an in-memory container
Database.getNotes().forEach(note => {
    notesFragment.appendChild(note.render());
});
notesContainer.appendChild(notesFragment);

/* Handler for creating a new note */
document.getElementById("create-note").addEventListener("click", () => 
{
    // Popup for the name
    const name = prompt("Enter the name of the new note:", "");
    if (name === null) { return; } // User cancelled prompt
    try {
        const note = new NoteItem({ name: name });
        Database.saveNote(note);
        note.open();
    } 
    catch (error) {
        alert(error.message); // e.g. "A note's name cannot be empty"
    }
});
