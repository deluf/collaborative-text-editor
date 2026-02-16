"use strict";

import { NoteItem } from "./noteItem.js";

export { Database };

/**
 * A static wrapper class for handling localStorage operations
 */
class Database {

    /**
     * Private enumeration of the localStorage keys used by the application
     * @private
     * @type {Object<string, string>}
     */
    static #KEYS = {
        USERNAME: "username",
        NOTES: "notes"
    };

    /**
     * Retrieves the current username from local storage.
     * If no username exists, generates a random integer ID, saves it, and returns it
     * @returns {string} The existing or newly generated username
     */
    static getUsername() {
        let currentUsername = localStorage.getItem(Database.#KEYS.USERNAME);
        // If the username is already defined use that, otherwise generate it as a random integer 
        if (!currentUsername) {
            // An high enough value to (probabilistically) ensure uniqueness
            const usernameMax = 1_000_000_000; 
            currentUsername = String(Math.floor(Math.random() * usernameMax));
            localStorage.setItem(Database.#KEYS.USERNAME, currentUsername);
        }
        return currentUsername;
    }

    /**
     * Updates the stored username
     * @param {string} username - The new username to store
     */
    static setUsername(username) {
        localStorage.setItem(Database.#KEYS.USERNAME, username);
    }

    /**
     * Retrieves all notes stored
     * @returns {Array<NoteItem>} An array of note objects. Returns an empty array if no notes are found
     */
    static getNotes() {
        const notesJson = localStorage.getItem(Database.#KEYS.NOTES);
        if (!notesJson) return [];
        const plainNotes = JSON.parse(notesJson);
        // Map each plain object to a new instance of NoteItem
        return plainNotes.map(noteData => new NoteItem(noteData));
    }

    /**
     * Adds a new note (or updates an existing one)
     * @param {NoteItem} note - The note object to save
     */
    static saveNote(note) {
        const notes = this.getNotes();
        const index = notes.findIndex(n => n.uuid === note.uuid);
        
        if (index >= 0) { notes[index] = note; } 
        else { notes.push(note); }
        
        localStorage.setItem(Database.#KEYS.NOTES, JSON.stringify(notes));
    }

    /**
     * Permanently removes a note from local storage
     * @param {string} uuid - The unique identifier of the note to delete
     */
    static deleteNote(uuid) {
        const notes = this.getNotes();
        const filteredNotes = notes.filter(n => n.uuid !== uuid);
        localStorage.setItem(Database.#KEYS.NOTES, JSON.stringify(filteredNotes));
    }

    /**
     * Renames an existing note
     * @param {string} uuid - The unique identifier of the note to rename
     * @param {string} newName - The new name to assign to the note
     * @returns {string} The trimmed new name
     * @throws {Error} If the new name is empty/whitespace only
     * @throws {Error} If a note with the provided UUID does not exist
     */
    static renameNote(uuid, newName) {
        if (!newName || newName.trim() === "") {
            throw new Error("A note's name cannot be empty");
        }
        
        const notes = this.getNotes();
        const note = notes.find(n => n.uuid === uuid);
        
        if (!note) {
            throw new Error("The selected note does not exist");
        }

        const trimmedName = newName.trim();
        note.name = trimmedName;
        
        localStorage.setItem(Database.#KEYS.NOTES, JSON.stringify(notes));
        return trimmedName;
    }

}
