'use strict'
const Elm = require('./build/app/app.js')

// get a reference to the div where we will show our UI
let container = document.getElementById('body')

// start the elm app in the container
// and keep a reference for communicating with the app
let feldspar = Elm.Main.embed(container)