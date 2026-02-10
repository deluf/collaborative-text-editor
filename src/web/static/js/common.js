const LOCAL_STORAGE_KEYS = {
    DISPLAY_NAME: 'display-name',
    DOCUMENTS: 'documents'
};

function getLocalDocuments() {
    const documents = localStorage.getItem(LOCAL_STORAGE_KEYS.DOCUMENTS);
    try { return documents ? JSON.parse(documents) : []; } 
    catch (error) {
        console.error("Error parsing documents from local storage", error);
        return [];
    }
}
