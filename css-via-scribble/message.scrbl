#lang scribble/text
@(require "style.ss" "vars.ss" "utils.ss" "styles.ss")
<html>
  <body>
    <img src='@asset{header.gif}' alt='Header Graphic'/>
    <p>
      Want to visit <a href='@|project-url|'>@|project-url|</a>.
      We <span @class{loud}>rock</a>!
</p>
</body>
</html>
