"use strict";

import { NoteView } from "./noteView.js";
import { NoteItem } from "./noteItem.js";
import { Database } from "./database.js";
import { RemoteCursorManager } from "./remoteCursorManager.js";
import { FractionalIdManager } from "./fractionalIdManager.js";
import { CollaborativeSocketClient, ACTION, EditMessage, SyncMessage } from "./collaborativeSocketClient.js";

/**
 * @fileoverview Main client-side controller for the collaborative note editor
 */

/*  --- Global variables --- */

const USERNAME = Database.getUsername();

const protocol = window.location.protocol === "https:" ? "wss" : "ws";
const hostname = "10.2.1.24" // Load balancer
const port = 8086;
const openNote = loadNoteOrCreateIfNew();

const NOTE_VIEW = new NoteView(
    openNote, onLocalInsert, onLocalDelete, onLocalMove
);

const SOCKET = new CollaborativeSocketClient(
    protocol, hostname, port, openNote.uuid,
    processIncomingEditMessage, processIncomingSyncMessage, NOTE_VIEW.setConnectionStatus
);

const FRACTIONAL_ID_MANAGER = new FractionalIdManager();

const CURSOR_MANAGER = new RemoteCursorManager(
    NOTE_VIEW.GUI.textArea,
    NOTE_VIEW.GUI.overlay,
    NOTE_VIEW.GUI.ghostTextArea
);

/**
 * Loads the requested note using URL search parameters.
 * If the note doesn't exist locally but parameters are provided, it imports and saves it.
 * If no valid parameters or existing note are found, redirects the user to the homepage
 * @returns {NoteItem} The initialized note item
 */
function loadNoteOrCreateIfNew() {
    // .../note?uuid=...&name=...
    const params = new URLSearchParams(window.location.search);
    const uuid = params.get("uuid");
    const name = params.get("name");

    const notes = Database.getNotes();
    let note = notes.find(note => note.uuid === uuid);

    // The note does not exist locally
    if (!note) { 
        // If the uuid and name are specified, import it 
        if ((uuid && name)) {
            note = new NoteItem({
                uuid: uuid,
                name: name,
                owned: false
            });
            Database.saveNote(note);
        }

        // Otherwise, abort
        else { window.location.href = "/"; }
    }

    return note;
}

/*  --- Callback functions --- */

/**
 * Handles a local character insertion event triggered by the UI.
 * Generates a fractional ID, registers it locally, and broadcasts the edit
 * @param {number} index - The zero-based index where the character was inserted
 */
function onLocalInsert(index) {
    const newId = FRACTIONAL_ID_MANAGER.getNewId(index);
    const edit = new EditMessage({
        username: USERNAME,
        action: ACTION.INSERT,
        id: newId,
        char: NOTE_VIEW.GUI.textArea.value[index]
    });
    index = FRACTIONAL_ID_MANAGER.insert(newId);
    if (index === -1) { 
        console.error(`Tried to insert duplicate ID "${newId}" - Ignoring the edit...`);
        return;
    }    
    SOCKET.sendEdit(edit);  
    CURSOR_MANAGER.overlayHeightSync();
    NOTE_VIEW.updateStats(USERNAME);
}

/**
 * Handles a local character deletion event triggered by the UI.
 * Retrieves the associated fractional ID, removes it locally, and broadcasts the edit
 * @param {number} index - The zero-based index of the deleted character
 */
function onLocalDelete(index) {
    const edit = new EditMessage({
        username: USERNAME,
        action: ACTION.DELETE,
        id: FRACTIONAL_ID_MANAGER.getIdFromIndex(index)
    });
    FRACTIONAL_ID_MANAGER.deleteFromIndex(index);
    SOCKET.sendEdit(edit);
    CURSOR_MANAGER.overlayHeightSync();
    NOTE_VIEW.updateStats(USERNAME);
}

/**
 * Handles a local cursor movement event triggered by the UI.
 * Resolves the fractional ID at the current cursor index and broadcasts the movement
 * @param {number} index - The new zero-based index of the local cursor
 */
function onLocalMove(index) {
    const edit = new EditMessage({
        username: USERNAME,
        action: ACTION.MOVE,
        id: FRACTIONAL_ID_MANAGER.getIdFromIndex(index)
    });
    SOCKET.sendEdit(edit);    
}

/**
 * Routes incoming edit messages from the WebSocket to the appropriate remote handler.
 * Ignores messages mirrored back to the sender
 * @param {EditMessage} edit - The edit message payload from a remote user
 */
function processIncomingEditMessage(edit) 
{
    if (edit.username === USERNAME) {
        console.warn("Ignoring mirrored update...");
        return;
    }

    console.debug("Received edit message: ", edit);
    switch (edit.action) {
        case ACTION.INSERT: onRemoteInsert(edit); break;
        case ACTION.DELETE: onRemoteDelete(edit); break;
        case ACTION.MOVE: onRemoteMove(edit); break;
        default:
            console.warn(`Received unknown action "${edit.action}" from user "${edit.username}"`);
            return;
    }
}

/**
 * Processes an incoming remote insertion edit.
 * Updates the local fractional ID manager, alters the text view, and shifts remote cursors
 * @param {EditMessage} edit - The insertion edit message payload
 */
function onRemoteInsert(edit) {
    const index = FRACTIONAL_ID_MANAGER.insert(edit.id);
    if (index === -1) { 
        console.error(`Tried to insert duplicate ID "${edit.id}" - ignoring the edit...`);
        return;
    }
    NOTE_VIEW.GUI.textArea.setRangeText(edit.char, index, index, "preserve");
    CURSOR_MANAGER.moveCursorByName(edit.username, index + 1);
    CURSOR_MANAGER.overlayHeightSync();
    NOTE_VIEW.updateStats(edit.username);
}

/**
 * Processes an incoming remote deletion edit.
 * Updates the local fractional ID manager, removes the character from the view, and shifts remote cursors
 * @param {EditMessage} edit - The deletion edit message payload
 */
function onRemoteDelete(edit) {
    const index = FRACTIONAL_ID_MANAGER.deleteFromId(edit.id);
    if (index === -1) { 
        console.error(`Tried to delete non-existent ID "${edit.id}" - ignoring the edit...`);
        return;
    }
    NOTE_VIEW.GUI.textArea.setRangeText("", index, index + 1, "preserve");
    CURSOR_MANAGER.moveCursorByName(edit.username, index);
    CURSOR_MANAGER.overlayHeightSync();
    NOTE_VIEW.updateStats(edit.username);
}

/**
 * Processes an incoming remote cursor movement edit
 * @param {EditMessage} edit - The movement edit message payload
 */
function onRemoteMove(edit) {
    const index = FRACTIONAL_ID_MANAGER.getIndexFromId(edit.id);
    CURSOR_MANAGER.moveCursorByName(edit.username, index);
}

/**
 * Handles a full document synchronization payload from the server.
 * Completely resets the local document state, rebuilds it from the payload, and places cursors
 * @param {SyncMessage} syncMessage - The complete state sync message from the server
 */
function processIncomingSyncMessage(syncMessage) {
    console.debug("Received sync message: ", syncMessage);
    CURSOR_MANAGER.clear();
    FRACTIONAL_ID_MANAGER.clear();
    NOTE_VIEW.GUI.textArea.value = "";
    
    // Populate document
    const document = [];
    for (const delta of syncMessage.data) {
        let index = FRACTIONAL_ID_MANAGER.insert(delta.id);
        if (index === -1) { 
            console.error(`Tried to insert duplicate ID "${delta.id}" - ignoring the edit...`);
            continue;
        }
        document.splice(index, 0, delta.char);
    }
    NOTE_VIEW.GUI.textArea.value = document.join("");

    // Populate cursors
    for (const cursor of syncMessage.cursors) {
        if (cursor.username === USERNAME) { continue; }
        const index = FRACTIONAL_ID_MANAGER.getIndexFromId(cursor.id);
        CURSOR_MANAGER.moveCursorByName(cursor.username, index);
    }
    NOTE_VIEW.updateStats("<SERVER>");
    CURSOR_MANAGER.overlayHeightSync();
}

