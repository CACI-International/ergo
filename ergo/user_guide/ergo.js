/*
Language: Ergo
*/
function ergo_lang(hljs) {
    return {
        name: "Ergo",
        keywords: "ergo pat fn index std workspace doc if",
        contains: [
            hljs.COMMENT(/#/, /$/, {
                contains: [
                    {
                        className: 'doc', begin: /##/, end: /$/
                    }
                ]
            }),
            hljs.QUOTE_STRING_MODE,
            {
                className: 'operator',
                begin: /(:|\^|!)/
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
