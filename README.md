# brick-tac-toe

Plan

- [x] simple echo server
- [ ] client side: button to send something to a socket and put it somewhere
    - [x] create socket at startup and keep a reference of it in the state
    - [ ] listener on the socket: push custom event into the channel
    - [ ] create handler for button: send something to the socket
    - [ ] create handler for custom socket events: put whatever is received in a text elem
- [ ] server: receive something and append '!'
