/*
Language: Ergo
*/
function ergo_lang(hljs) {
    return {
        name: "Ergo",
        keywords: "fn pat if",
        contains: [
            hljs.COMMENT(/#/, /$/),
            hljs.QUOTE_STRING_MODE,
            {
                className: 'type',
                begin: /(:|\^)/
            },
            {
                className: 'keyword',
                begin: /(\|>?|<\|)/
            }
        ]
    };
}

if (window.hljs) {
    window.hljs.registerLanguage("ergo", ergo_lang);
    window.hljs.initHighlighting();
}
