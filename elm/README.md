# Playing with mazes

I'm working my way through [Mazes for programmers](http://www.mazesforprogrammers.com/)
(VERY SLOWLY) and trying out some of the concepts in various languages.

This folder is for my mazes written in [elm](https://elm-lang.org/).

## Progress

I'm still in the early part of the book.
The mazes are very "move up and to the right".
But it's a lot of fun and I'm looking forward to implementing some of the more sophisticated algorithms.

### TODO

- [x] Binary Tree algorithm
- [x] Sidewinder algorithm
- [x] Resize board and cell size
- [x] Introduce a delay to watch the maze get carved
- [x] Solve the maze with arrow keys!
- [ ] The sliders are too finicky. Replace them with real number inputs?
- [ ] Don't redraw the maze on every input field change. Add a button?
- [ ] Remove walls by changing border color to match background (to fix one-pixel-off issues)
- [ ] More algorithms!

## Developent

```bash
npm install
npm run dev # live reloading dev server at http://localhost:8000/
npm run debug # dev server with debuger (slow)
npm run build # prod build to public/
```
