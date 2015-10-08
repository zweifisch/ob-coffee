var repl = require('repl');
repl.start({
    prompt: 'ob-coffee > ',
    input: process.stdin,
    output: process.stdout,
    ignoreUndefined: true,
    useColors: false,
    terminal: false
});
