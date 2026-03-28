const editor = document.getElementById('editor');
const output = document.getElementById('output');
const runBtn = document.getElementById('runBtn');
const clearBtn = document.getElementById('clearBtn');

runBtn.addEventListener('click', () => {
    output.textContent = 'Compilation not yet implemented.\n\nTo run CocoScript code:\n1. Install the compiler from GitHub\n2. Save your code to a .coco file\n3. Run: cocoscript yourfile.coco\n\nLive compilation coming soon!';
});

clearBtn.addEventListener('click', () => {
    output.textContent = '';
});

// Basic syntax highlighting (simple version)
editor.addEventListener('input', () => {
    // Could add syntax highlighting here later
});
