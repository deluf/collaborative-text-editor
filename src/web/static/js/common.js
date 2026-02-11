
export { LOCAL_STORAGE_KEYS, fetchUsername, Note };

const LOCAL_STORAGE_KEYS = {
    USERNAME: 'username',
    NOTES: 'notes'
};

function fetchUsername() {
    let currentUsername = localStorage.getItem(LOCAL_STORAGE_KEYS.USERNAME);
    // If the username is already defined use that, otherwise generate it as a random integer 
    if (!currentUsername) {
        currentUsername = String(Math.floor(Math.random() * 1_000_000_000));
        localStorage.setItem(LOCAL_STORAGE_KEYS.USERNAME, currentUsername);
    }
    return currentUsername;
}

class Note {

    /**
     * @param {Object} data
     * @param {string} [data.uuid] - If null, a new UUID is generated
     * @param {string} data.name
     * @param {boolean} [data.owned=true]
     */
    constructor({ uuid, name, owned = true }) {
        if (!name || name.trim() === '') {
            throw new Error("A note's name cannot be empty");
        }

        this.uuid = uuid || self.crypto.randomUUID();
        this.name = name.trim();
        this.owned = owned;
    }

    /**
     * Generates the HTML element for this note
     * @returns {HTMLAnchorElement}
     */
    render() {    
        const link = document.createElement('a');
        link.href = `/note?uuid=${this.uuid}`;
        
        const icon = document.createElement('div');
        icon.className = this.owned ? 'owned-note-icon' : 'shared-note-icon';

        const name = document.createElement('div');
        name.textContent = this.name;

        link.appendChild(icon);
        link.appendChild(name);
        
        return link;
    }

    /**
     * Generates the shareable link for this note
     * @returns {string}
     */
    getShareURL() {
        const origin = window.location.origin; 
        const safeUUID = encodeURIComponent(this.uuid);
        const safeName = encodeURIComponent(this.name);
        return `${origin}/note?uuid=${safeUUID}&name=${safeName}`;
    }

    /**
     * Redirects the browser to this note
     */
    open() {
        window.location.href = `/note?uuid=${this.uuid}`;
    }

    /**
     * Saves the current note instance to LocalStorage
     */
    save() {
        const notes = Note.#getAllRaw();
        notes.push(this.#toJSON());
        localStorage.setItem(LOCAL_STORAGE_KEYS.NOTES, JSON.stringify(notes));
    }

    /**
     * Serializes the object for storage
     */
    #toJSON() {
        return {
            uuid: this.uuid,
            name: this.name,
            owned: this.owned
        };
    }

    /**
     * Retrieves all notes from storage as instances of Note
     * @returns {Note[]}
     */
    static getAll() {
        return this.#getAllRaw().map(data => new Note(data));
    }

    /**
     * Low-level retrieval from LocalStorage
     * @returns {Array<Object>}
     */
    static #getAllRaw() {
        const storedNotes = localStorage.getItem(LOCAL_STORAGE_KEYS.NOTES);
        return storedNotes ? JSON.parse(storedNotes) : [];
    }

    static delete(uuid) {
        const notes = this.#getAllRaw();
        const filteredNotes = notes.filter(note => note.uuid !== uuid);
        localStorage.setItem(
            LOCAL_STORAGE_KEYS.NOTES,
            JSON.stringify(filteredNotes)
        );
    }

    static rename(uuid, newName) {
        if (!newName || newName.trim() === '') {
            throw new Error("A note's name cannot be empty");
        }

        const notes = this.#getAllRaw();
        const trimmedName = newName.trim();
        const note = notes.find(n => n.uuid === uuid);

        if (!note) { 
            throw new Error("The selected note does not exists");
        }

        note.name = trimmedName;
        localStorage.setItem(
            LOCAL_STORAGE_KEYS.NOTES,
            JSON.stringify(notes)
        );

        return newName;
    }

}
