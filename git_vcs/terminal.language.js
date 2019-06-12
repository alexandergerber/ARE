/*
  Language: terminal console
  Author: Josh Bode <joshbode@gmail.com>
*/

// extract the embedded styling from ansi spans
  var highlighted = document.querySelectorAll("code.terminal span.hljs-ansi");
  Array.prototype.forEach.call(highlighted, function(next) {
    next.insertAdjacentHTML("beforebegin", next.textContent);
    next.parentNode.removeChild(next);
  });

hljs.registerLanguage('terminal', function() {
  return {
    contains: [
      {
        className: 'string',
        begin: '^([\\w.]+)@([\\w.]+)'
      },
      {
        className: 'constant',
        begin: ' (.*) \\$ '
      },
      {
        className: 'ansi',
        begin: '<span style\\="([^"]+)">',
        end: '<\\/span>'
      }
    ]
  }
});
