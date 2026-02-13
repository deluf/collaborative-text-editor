"use strict";

import { NoteView } from "./noteView.js";
import { NoteItem } from './noteItem.js';
import { Database } from './database.js';
import { RemoteCursorManager } from "./remoteCursorManager.js";
import { FractionalIdManager } from "./fractionalIdManager.js";
import { CollaborativeSocketClient, EditMessage, SyncMessage } from "./collaborativeSocketClient.js";

/*  --- Global variables --- */

const USERNAME = Database.getUsername();

const protocol = window.location.protocol === 'https:' ? 'wss' : 'ws';
const hostname = window.location.hostname;
const port = 8086;
const openNote = loadNoteOrCreateIfNew();

const SOCKET = new CollaborativeSocketClient(
    protocol, hostname, port, openNote.uuid,
    processIncomingEditMessage, processIncomingSyncMessage
);

const NOTE_VIEW = new NoteView(
    USERNAME, openNote,
    onLocalInsert, onLocalDelete, onLocalMove
);

const FRACTIONAL_ID_MANAGER = new FractionalIdManager();

const CURSOR_MANAGER = new RemoteCursorManager(
    NOTE_VIEW.GUI.textArea,
    NOTE_VIEW.GUI.overlay,
    NOTE_VIEW.GUI.ghostTextArea
);



function loadNoteOrCreateIfNew() {
    // .../note?uuid=...&name=...
    const params = new URLSearchParams(window.location.search);
    const uuid = params.get('uuid');
    const name = params.get('name');

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
        else { window.location.href = '/'; }
    }

    return note;
}






/*  --- Callback functions --- */

function onLocalInsert(index) {
    const newId = FRACTIONAL_ID_MANAGER.getNewId(index);
    const edit = new EditMessage({
        username: USERNAME,
        action: "insert", // FIXME: maybe an enum?
        id: newId,
        char: NOTE_VIEW.GUI.textArea.value[index]
    });
    FRACTIONAL_ID_MANAGER.insert(newId);
    if (index === -1) { 
        console.error(`Tried to insert duplicate ID '${newId}' - Ignoring the edit...`);
        return;
    }    
    SOCKET.sendEdit(edit);  
    CURSOR_MANAGER.overlayHeightSync();
}

function onLocalDelete(index) {
    const edit = new EditMessage({
        username: USERNAME,
        action: "delete",
        id: FRACTIONAL_ID_MANAGER.getIdFromIndex(index)
    });
    FRACTIONAL_ID_MANAGER.deleteFromIndex(index);
    SOCKET.sendEdit(edit);
    CURSOR_MANAGER.overlayHeightSync();
}

function onLocalMove(index) {
    const edit = new EditMessage({
        username: USERNAME,
        action: "move",
        id: FRACTIONAL_ID_MANAGER.getIdFromIndex(index)
    });
    SOCKET.sendEdit(edit);    
}

function processIncomingEditMessage(edit) 
{
    if (edit.username === USERNAME) {
        console.warn("Ignoring mirrored update...");
        return;
    }

    console.info("Received edit message: ", edit);
    switch (edit.action) {
        case 'insert': onRemoteInsert(edit); break;
        case 'delete': onRemoteDelete(edit); break;
        case 'move': onRemoveMove(edit); break;
        default:
            console.warn(`Received unknown action '${edit.action}' from user '${edit.username}'`);
            return;
    }
}

function onRemoteInsert(edit) {
    const index = FRACTIONAL_ID_MANAGER.insert(edit.id);
    if (index === -1) { 
        console.error(`Tried to insert duplicate ID '${edit.id}' - ignoring the edit...`);
        return;
    }
    NOTE_VIEW.GUI.textArea.setRangeText(edit.char, index, index, 'preserve');
    CURSOR_MANAGER.moveCursorByName(edit.username, index + 1);
    CURSOR_MANAGER.overlayHeightSync();
    NOTE_VIEW.updateStats(edit.username);
}

function onRemoteDelete(edit) {
    const index = FRACTIONAL_ID_MANAGER.deleteFromId(edit.id);
    if (index === -1) { 
        console.error(`Tried to delete non-existent ID '${edit.id}' - ignoring the edit...`);
        return;
    }
    NOTE_VIEW.GUI.textArea.setRangeText('', index, index + 1, 'preserve');
    CURSOR_MANAGER.moveCursorByName(edit.username, index);
    CURSOR_MANAGER.overlayHeightSync();
    NOTE_VIEW.updateStats(edit.username);
}

function onRemoveMove(edit) {
    const index = FRACTIONAL_ID_MANAGER.getIndexFromId(edit.id);
    CURSOR_MANAGER.moveCursorByName(edit.username, index);
}

/**
 * 
 * @param {SyncMessage} syncMessage
 */
function processIncomingSyncMessage(syncMessage) {
    console.info("Received sync message: ", syncMessage);
    CURSOR_MANAGER.clear();
    FRACTIONAL_ID_MANAGER.clear();
    NOTE_VIEW.GUI.textArea.value = "";
    for (const delta of syncMessage.data) {
        let index = FRACTIONAL_ID_MANAGER.insert(delta.id);
        if (index === -1) { 
            console.error(`Tried to insert duplicate ID '${delta.id}' - ignoring the edit...`);
            continue;
        }
        NOTE_VIEW.GUI.textArea.setRangeText(delta.char, index, index, 'end');
    }
    NOTE_VIEW.updateStats("<SERVER>");
    CURSOR_MANAGER.overlayHeightSync();
}


// FIXME: cursors on sync