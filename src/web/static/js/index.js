// Handle display name
const displayNameInput = document.getElementById('display-name');
const currentDisplayName = localStorage.getItem(LOCAL_STORAGE_KEYS.DISPLAY_NAME) ??
    String(Math.floor(Date.now() / 1000));
displayNameInput.value = currentDisplayName;
displayNameInput.addEventListener('input', (event) => {
    localStorage.setItem(LOCAL_STORAGE_KEYS.DISPLAY_NAME, event.target.value);
});

// Render existing documents
// Document structure: { uuid: string, filename: string, owned: boolean }
const documents = getLocalDocuments();
const documentsContainer = document.getElementById('documents');
documents.forEach(doc => 
{
    const link = document.createElement('a');
    link.href = `/document?id=${doc.uuid}`;
    
    const icon = document.createElement('div');
    icon.className = doc.owned ? 'owned-document-icon' : 'shared-document-icon';

    const name = document.createElement('div');
    name.textContent = doc.filename;

    link.appendChild(icon);
    link.appendChild(name);
    documentsContainer.appendChild(link);
});

// Create new document
document.getElementById('create-document').addEventListener('click', () => 
{
    // Popup for the filename
    const filename = prompt('Enter the name of the new note:', '');
    if (filename === null || filename.trim() === '') { return; }  

    const uuid = self.crypto.randomUUID();
    const newDocument = {
        uuid: uuid,
        filename: filename,
        owned: true 
    };

    // Add to local storage
    const currentDocs = getLocalDocuments();
    currentDocs.push(newDocument);
    localStorage.setItem(LOCAL_STORAGE_KEYS.DOCUMENTS, JSON.stringify(currentDocs));

    // Open the created document
    window.location.href = `/document?id=${uuid}`;
});

