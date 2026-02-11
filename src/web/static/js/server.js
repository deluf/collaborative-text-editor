
export { Server, Edit };

class Server {
    constructor() {
        this.url = 'ws://localhost:8765'
        // Bind the onReceive function to see 'this' as the Server class, not the WebSocke class
        this.onReceive = this.onReceive.bind(this);
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
            console.info("Received:", edit);
            
            processIncomingRequest(edit); // FIXME: edit that function to receive an edit
        } 
        catch (error) {
            console.error(`Error parsing incoming message: ${error}`);
        }
    }

    /**
     * @param {Edit} edit
     */
    send(edit) {
        // edit.id can be undefined if for example the user is moving after the last char, 
        //  or if the textarea is empty FIXME:
        if (!edit.id) { 
            console.warn("Id undefined, changing it to '<<EOF>>'")
            edit.id = "<<EOF>>";
        }
        if (this.socket.readyState === WebSocket.OPEN) {
            const jsonString = JSON.stringify(edit);
            this.socket.send(jsonString);
            console.info("Sent: ", edit);
        } else {
            console.error(`Socket ${this.url} is closed`);
        }
    }
}

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
