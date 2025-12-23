const DISPLAY_NAME_KEY = 'displayname';
const DOCUMENTS_KEY = 'documents';

// Handle display name
const displayNameInput = document.getElementById('displayname');
const savedDisplayname = localStorage.getItem(DISPLAY_NAME_KEY);
if (savedDisplayname) { displayNameInput.value = savedDisplayname; }
displayNameInput.addEventListener('input', (event) => {
    localStorage.setItem(DISPLAY_NAME_KEY, event.target.value);
});

// Render existing documents
function getLocalDocuments() {
    const documents = localStorage.getItem(DOCUMENTS_KEY);
    // Structure: { uuid: string, filename: string, owned: boolean }
    try {
        return documents ? JSON.parse(documents) : [];
    } catch (e) {
        console.error("Error parsing documents from local storage", e);
        return [];
    }
}
const documents = getLocalDocuments();
const documentsDiv = document.getElementById('documents');
documents.forEach(doc => {
    const link = document.createElement('a');
    link.href = `/document?id=${doc.uuid}`;

    const icon = document.createElement('div');
    icon.className = doc.owned ? 'owned-document' : 'shared-document';

    const name = document.createElement('div');
    name.className = 'document-name';
    name.textContent = doc.filename;

    link.appendChild(icon);
    link.appendChild(name);
    documentsDiv.appendChild(link);
});

// Create new document
document.getElementById('create-document').addEventListener('click', () => 
{
    // Popup for the filename
    const filename = prompt('Enter the name of the new note:', '');
    if (filename === null) { return; }

    const uuid = self.crypto.randomUUID();
    const newDoc = {
        uuid: uuid,
        filename: filename,
        owned: true 
    };

    // Add to local storage
    const currentDocs = getLocalDocuments();
    currentDocs.push(newDoc);
    localStorage.setItem(DOCUMENTS_KEY, JSON.stringify(currentDocs));

    // Open the created document
    window.location.href = `/document?id=${uuid}`;
});

