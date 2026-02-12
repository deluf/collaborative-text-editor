import Ajv from "https://esm.sh/ajv";

export { Server, Edit, Sync };

/**
 * Manages the WebSocket connection for real-time collaboration on a specific note.
 * This class handles:
 * - Establishing and maintaining WebSocket connections.
 * - Automatic reconnection with exponential backoff strategies.
 * - Validation of incoming messages using JSON schemas.
 * - Queuing local edits when the connection is offline.
 */
class Server {

    /**
     * Creates an instance of the Server.
     * * @param {string} noteUUID - The unique identifier of the note to connect to.
     * @param {function(Edit): void} onEditMessageReceived - Callback function invoked when a valid edit message is received.
     * @param {function(Sync): void} onSyncMessageReceived - Callback function invoked when a valid sync message is received.
     */
    constructor(noteUUID, onEditMessageReceived, onSyncMessageReceived) {
        const hostname = window.location.hostname;
        const port = 8086;
        this.url = `ws://${hostname}:${port}/${noteUUID}`;

        this.onEditMessageReceived = onEditMessageReceived;
        this.onSyncMessageReceived = onSyncMessageReceived;

        // Bind methods to ensure 'this' refers to the Server instance, not the WebSocket class
        this.onReceive = this.onReceive.bind(this);
        this.onOpen = this.onOpen.bind(this);
        this.onClose = this.onClose.bind(this);

        // Configuration for reconnection strategy
        this.BASE_RECONNECT_DELAY_MS = 1000; // Start with 1 second
        this.MAX_RECONNECT_DELAY_MS = 30000; // Cap at 30 seconds
        this.BACKOFF_FACTOR = 2;             // Exponential backoff

        this.reconnectDelay = this.BASE_RECONNECT_DELAY_MS;
        this.reconnectTimeoutId = null;

        /** @type {Edit[]} */
        this.editsQueue = [];

        const ajv = new Ajv();
        this.validateSyncMessage = ajv.compile(syncMessageSchema);
        this.validateEditMessage = ajv.compile(editMessageSchema);

        this.#connect();
    }

    /**
     * Establishes the WebSocket connection and binds the event listeners.
     * @private
     */
    #connect() {
        // Clean up existing socket listeners if they exist
        if (this.socket) {
            this.socket.removeEventListener('open', this.onOpen);
            this.socket.removeEventListener('message', this.onReceive);
            this.socket.removeEventListener('close', this.onClose);
            this.socket.close();
        }
        this.socket = new WebSocket(this.url);
        this.socket.addEventListener('open', this.onOpen);
        this.socket.addEventListener('message', this.onReceive);
        this.socket.addEventListener('close', this.onClose);
    }

    /**
     * Event handler for the WebSocket 'open' event.
     * Resets reconnection delays and attempts to flush any queued edits.
     * @private
     */
    onOpen() {
        console.info(`Connected to ${this.url}`);
        
        // Clear the reconnect timer
        if (this.reconnectTimeoutId) {
            clearTimeout(this.reconnectTimeoutId);
            this.reconnectTimeoutId = null;
        }

        // Reset the reconnection delay
        this.reconnectDelay = this.BASE_RECONNECT_DELAY_MS;
        
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
     * Event handler for the WebSocket 'close' event.
     * Initiates the reconnection process using an exponential backoff strategy.
     * @private
     */
    onClose() {
        console.warn(`Socket closed unexpectedly - reconnecting in ${this.reconnectDelay} ms ...`);
        this.reconnectTimeoutId = setTimeout(() => {
            this.reconnectDelay = Math.min(this.reconnectDelay * this.BACKOFF_FACTOR, this.MAX_RECONNECT_DELAY_MS);
            this.#connect();
        }, this.reconnectDelay);
    }

    /**
     * Event handler for the WebSocket 'message' event.
     * Parses the incoming JSON, validates it against known schemas, 
     * and triggers the appropriate callback.
     * * @param {MessageEvent} event - The WebSocket message event.
     */
    onReceive(event) {
        let data;
        try { data = JSON.parse(event.data); } 
        catch (error) { console.error(`Error parsing incoming message: ${error}`); }

        if (this.validateEditMessage(data)) { this.onEditMessageReceived(data); }
        else if (this.validateSyncMessage(data)) { this.onSyncMessageReceived(data); }
        else {
            console.warn("Received unknown message format: ", data);
            return;
        }
    }

    /**
     * Sends an Edit object to the server.
     * If the socket is closed, the edit is queued to be sent upon reconnection.
     * * @param {Edit} edit - The edit object to send.
     */
    sendEdit(edit) {
        if (!edit.id) {
            console.warn("Got an undefined id, aborting the send...", edit);
            return;
        }

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

}

/**
 * JSON Schema for validating individual Edit messages.
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
    additionalProperties: false
};

/**
 * Represents a discrete edit operation (insert, delete, or move) on the document.
 */
class Edit {
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
 * JSON Schema for validating Sync messages containing the current state of the document.
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
        }
    },
    required: ["action", "data"],
    additionalProperties: false
};

/**
 * Represents a synchronization event containing the current state of the document.
 */
class Sync {
    /**
     * @param {Object} parameters
     * @param {'sync'} parameters.action - 
     * @param {Array<{id: string, char: string}>} parameters.data 
     */
    constructor({ action = 'sync', data }) {
        this.action = action;
        this.data = data;
    }
}
