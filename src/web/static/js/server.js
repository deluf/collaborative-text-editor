export { Server, Edit };

class Server {
    /**
     * 
     * @param {(edit: Edit) => void} onReceiveCallback 
     */
    // FIXME: reconnection? (timeout is 60s on erlang)
    // FIXME: bug strano del client dopo una delete ricopia tutto??
    constructor(noteUuid, onReceiveCallback) {
        this.url = `ws://localhost:8086/${noteUuid}`;
        // Bind the onReceive function to see 'this' as the Server class, not the WebSocket class
        this.onReceive = this.onReceive.bind(this);
        this.onReceiveCallback = onReceiveCallback;
        this.socket = new WebSocket(this.url);
        this.socket.addEventListener('open', () => { console.info(`Connected to ${this.url}`); });
        this.socket.addEventListener('message', this.onReceive);
    }

    onReceive(event) {
        try {
            const data = JSON.parse(event.data);
            // Check if the data looks like an Edit
            if (!data.username || !data.action || !data.id) {
                console.warn("Received unknown data format: ", data);
                return
            }
            const edit = new Edit(data);
            console.info("Received: ", edit);
            this.onReceiveCallback(edit);
        } 
        catch (error) {
            console.error(`Error parsing incoming message: ${error}`);
        }
    }

    /**
     * @param {Edit} edit
     */
    sendEdit(edit) {
        if (!edit.id) { 
            console.warn("Got an undefined id, treating it as '<<EOF>>'")
            edit.id = "<<EOF>>";
        }
        console.info("Sending: ", edit);
        if (this.socket.readyState === WebSocket.OPEN) {
            const jsonString = JSON.stringify(edit);
            this.socket.send(jsonString);
        } else {
            console.error(`Socket ${this.url} is closed`);
        }
    }

    // FIXME: c'è già il timeout di cowboy...
    sendHeartBeat(username) {
        const jsonString = JSON.stringify({
            username: username,
            action: "HEARTBEAT"
        });
        this.socket.send(jsonString);
    }

    // FIXME: non penso serva
    requestFullNote() {
        const jsonString = JSON.stringify({
            action: "sync"
        });
        this.socket.send(jsonString);
    }
}

// FIXME: forse meglio msg generico?
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
