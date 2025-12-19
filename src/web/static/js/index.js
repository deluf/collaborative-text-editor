
const DISPLAY_NAME_INPUT = document.getElementById('displayname');

const DISPLAY_NAME_KEY = 'displayname';

const savedDisplayname = localStorage.getItem(DISPLAY_NAME_KEY);
if (savedDisplayname) {
    DISPLAY_NAME_INPUT.value = savedDisplayname;
}

DISPLAY_NAME_INPUT.addEventListener('input', (event) => {
    localStorage.setItem(DISPLAY_NAME_KEY, event.target.value);
});

