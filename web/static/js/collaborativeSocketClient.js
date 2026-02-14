"use strict";

import Ajv from "https://esm.sh/ajv";

export { CollaborativeSocketClient, EditMessage, SyncMessage };

/**
 * Manages the WebSocket connection to a server for real-time collaboration on a specific note.
 * This class handles:
 * - Establishing and maintaining WebSocket connections
 * - Automatic reconnection with exponential backoff strategies
 * - Queuing local edits when the connection is offline
 * - Validation of incoming messages using JSON schemas
 */
class CollaborativeSocketClient {

    /** @private Configuration constants for the reconnection strategy */
    static #BASE_RECONNECT_DELAY_MS = 1000; // Start with 1 second
    static #MAX_RECONNECT_DELAY_MS = 30000; // Cap at 30 seconds
    static #BACKOFF_FACTOR = 2;             // Exponential backoff

    /**
     * Creates an instance of the CollaborativeSocketClient
     * @param {string} protocol - The WebSocket protocol to use (e.g. "ws" or "wss")
     * @param {string} hostname - The server hostname or IP address to connect to
     * @param {number} port - The port number of the WebSocket server
     * @param {string} noteUUID - The unique identifier of the note to connect to
     * @param {function(EditMessage): void} onEditMessageReceived - Callback function invoked when
     *  a valid edit message is received
     * @param {function(SyncMessage): void} onSyncMessageReceived - Callback function invoked when
     *  a valid sync message is received
     */
    constructor(protocol, hostname, port, noteUUID, onEditMessageReceived, onSyncMessageReceived) {
        this.url = `${protocol}://${hostname}:${port}/${noteUUID}`;
        this.onEditMessageReceived = onEditMessageReceived;
        this.onSyncMessageReceived = onSyncMessageReceived;

        this.reconnectDelay = CollaborativeSocketClient.#BASE_RECONNECT_DELAY_MS;
        this.reconnectTimeoutId = null;

        /** @type {EditMessage[]} */
        this.editsQueue = [];

        const ajv = new Ajv();
        this.validateSyncMessage = ajv.compile(syncMessageSchema);
        this.validateEditMessage = ajv.compile(editMessageSchema);

        this.#connect();
    }

    /**
     * Sends an EditMessage object to the server.
     * If the socket is closed, the edit is queued to be sent upon reconnection
     * @param {EditMessage} edit - The edit object to send
     */
    sendEdit(edit) {
        if (this.socket.readyState === WebSocket.OPEN) {
            console.info("Sending: ", edit);
            const jsonString = JSON.stringify(edit);
            this.socket.send(jsonString);
        } 
        else {
            this.editsQueue.push(edit);
            console.warn(`Unable to send edit: socket ${this.url} is closed.` + 
                `There are ${this.editsQueue.length} edits in queue`);
        }
    }

    /**
     * Establishes the WebSocket connection and binds the event listeners
     * @private
     */
    #connect() {
        // Clean up existing socket listeners if they exist
        if (this.socket) {
            this.socket.removeEventListener('open', this.#onOpen);
            this.socket.removeEventListener('message', this.#onReceive);
            this.socket.removeEventListener('close', this.#onClose);
            this.socket.close();
        }
        this.socket = new WebSocket(this.url);
        this.socket.addEventListener('open', this.#onOpen);
        this.socket.addEventListener('message', this.#onReceive);
        this.socket.addEventListener('close', this.#onClose);
    }

    /**
     * Event handler for the WebSocket 'open' event
     * Resets reconnection delays and attempts to flush any queued edits
     * @private
     */
    #onOpen = () => {
        console.info(`Connected to ${this.url}`);
        
        // Clear the reconnect timer
        if (this.reconnectTimeoutId) {
            clearTimeout(this.reconnectTimeoutId);
            this.reconnectTimeoutId = null;
        }

        // Reset the reconnection delay
        this.reconnectDelay = CollaborativeSocketClient.#BASE_RECONNECT_DELAY_MS;
        
        // Flush queued edits (if any)
        if (this.editsQueue.length > 0) {
            console.info(`Socket reconnected - sending ${this.editsQueue.length} edits in queue...`);
            for (const edit of this.editsQueue) {
                this.sendEdit(edit);
            }
            this.editsQueue = [];
        }
    }

    /**
     * Event handler for the WebSocket 'close' event
     * Initiates the reconnection process using an exponential backoff strategy
     * @private
     */
    #onClose = () => {
        console.warn(`Socket closed unexpectedly - reconnecting in ${this.reconnectDelay} ms ...`);
        this.reconnectTimeoutId = setTimeout(() => {
            this.reconnectDelay = Math.min(
                this.reconnectDelay * CollaborativeSocketClient.#BACKOFF_FACTOR,
                CollaborativeSocketClient.#MAX_RECONNECT_DELAY_MS
            );
            this.#connect();
        }, this.reconnectDelay);
    }

    /**
     * Event handler for the WebSocket 'message' event.
     * Parses the incoming JSON, validates it against known schemas, 
     *  and triggers the appropriate callback
     * * @param {MessageEvent} event - The WebSocket message event
     */
    #onReceive = (event) => {
        let data;
        try { data = JSON.parse(event.data); } 
        catch (error) { console.error(`Error parsing incoming message: ${error}`); }

        if (this.validateEditMessage(data)) { this.onEditMessageReceived(data); }
        else if (this.validateSyncMessage(data)) { this.onSyncMessageReceived(data); }
        else { console.warn("Received unknown message format: ", data); return; }
    }

}

/**
 * JSON Schema for validating Edit messages
 */
const editMessageSchema = {
    type: "object",
    properties: {
        action: { 
            type: "string",
            enum: ["insert", "delete", "move"] 
        },
        username: { "type": "string" },
        id: { "type": "string" },
        char: { "type": "string", "minLength": 1, "maxLength": 1 }
    },
    required: ["action", "username", "id"],
    additionalProperties: false,
    // Enforce the presence of "char" field if the action is an "insert"
    if: { properties: { action: { const: "insert" } } },
    then: { required: ["char"] }
};

/**
 * Represents a discrete edit operation (insert, delete, or move) on the document
 */
class EditMessage {
    /**
     * @param {Object} parameters
     * @param {string} parameters.username - The user performing the edit
     * @param {string} parameters.action - One of: 'insert', 'delete', 'move'
     * @param {string} parameters.id - The fractional identifier of the edited char
     * @param {string|null} [parameters.char=null] - The ASCII character being inserted (if any)
     */
    constructor({ username, action, id, char = null }) {
        this.username = username;
        this.action = action;
        this.id = id;
        this.char = char;
    }
}

/**
 * JSON Schema for validating Sync messages
 */
const syncMessageSchema = {
    type: "object",
    properties: {
        action: { 
            type: "string",
            enum: ["sync"] 
        },
        data: {
            type: "array",
            items: {
                type: "object",
                properties: {
                    id: { "type": "string" },
                    char: { "type": "string", "minLength": 1, "maxLength": 1 }
                },
                required: ["id", "char"],
                additionalProperties: false
            }
        },
        cursors: {
            type: "array",
            items: {
                type: "object",
                properties: {
                    id: { "type": "string" },
                    username: { "type": "string" }
                },
                required: ["id", "username"],
                additionalProperties: false
            }
        }
    },
    required: ["action", "data", "cursors"],
    additionalProperties: false
};

/**
 * Represents a synchronization event containing the current state of the whole document
 */
class SyncMessage {
    /**
     * @param {Object} parameters
     * @param {'sync'} parameters.action - 
     * @param {Array<{id: string, char: string}>} parameters.data 
     * @param {Array<{id: string, username: string}>} parameters.cursors
     */
    constructor({ action = 'sync', data, cursors }) {
        this.action = action;
        this.data = data;
        this.cursors = cursors;
    }
}
