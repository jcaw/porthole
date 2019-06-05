<p align=center>
    <img src="media/logo.png" alt="porthole logo" />
</p>

<h1 align=center>Porthole</h1>

<p align=center>An RPC server for Emacs. Control Emacs from outside Emacs, using HTTP.</p>

<p align=center>
<!-- This is an emoticon. Don't delete it if it doesn't show up in your editor. -->
📡
</p>

---

<!-- ## What is this Package? -->


This server allows Elisp functions to be invoked via HTTP requests. It uses the
[JSON-RPC 2.0](https://www.jsonrpc.org/specification) protocol.

It's easy to send RPC calls. Just POST some JSON. Here's an example in Python:

```python
# A simple example that shows an Emacs RPC request in Python.
import requests

rpc_call = {
    "jsonrpc": "2.0"
    "method": "insert",
    "params": ["This is some text we want to insert"],
    "id": 23084
}
# The server is running on port 8000.
response = requests.post("localhost:8000", json=rpc_call)
print(response.json())
```

Voila. The text will be inserted into the current buffer in Emacs. The return
value from the operation will be sent back to Python.

---


<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc 

    Please note that the markdown generator doesn't work perfectly with a
    centered heading, as above. It will need manual tweaking -->

**Table of Contents**

- [Why?](#why)
- [Installation](#installation)
- [Usage Overview](#usage-overview)
- [The JSON-RPC 2.0 Protocol](#the-json-rpc-20-protocol)
- [Usage Examples](#usage-examples)
    - [Example 1: Automatic Setup](#example-1-automatic-setup)
    - [Example 2: Manual Setup, with Authentication](#example-2-manual-setup-with-authentication)
- [Publishing Functions](#publishing-functions)
- [Outside Localhost](#outside-localhost)
- [FAQ](#faq)


<!-- markdown-toc end -->

## Why?

I want to open Emacs up. An Emacs session has a lot happening, and it's all
text. What if you could gain access to that information?

What if you could:

- Edit the contents of any text box [with Emacs?](https://github.com/cknadler/vim-anywhere)
- Interact with Emacs from a Python REPL?
- Control Emacs with something other than a keyboard, like [your voice?](https://www.youtube.com/watch?v=8SkdfdXWYaI)
- Use Emacs' outstanding editing tools *outside Emacs?*

All of these need some way of communicating with Emacs. An RPC server acts as a
foundation that allows Emacs to expose its functionality *in a way that's simple.*

## Installation

It will be installable from MELPA once I persuade them to add it (and the
[`json-rpc-server`](http://github.com/jcaw/json-rpc-server.el)).

## Usage Overview

Just register the commands you want to be available, start the server, and make
requests via HTTP. Functions will be executed in the current Emacs session, and
the results returned to your client.

If you want a layer of protection, `porthole` has support for [Basic
Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication). It's
easy to enable. Just supply a username and/or password when starting the server.

Functions in this package are prefixed with `porthole-`.

## The JSON-RPC 2.0 Protocol

This server uses JSON-RPC 2.0 as the underlying protocol for executing functions. For instructions on how to structure JSON-RPC requests for Emacs, see the `json-rpc-server` package's README directly:

https://github.com/jcaw/json-rpc-server.el

## Usage Examples

Let's run through some usage examples.

### Example 1: Automatic Setup

Clients can be configured to connect automatically with no interaction from the
user, even when the server is using a dynamically allocated port. Let's set up a
dynamic server.

<b>In Emacs:</b> 

Start a server with the default configuration:

```emacs-lisp
;; Functions have to be exposed before they can be invoked remotely.
(jrpc-expose-function 'insert)
;; Start a server with the default configuration.
(porthole-start-server)
```

By default, the server will request a dynamic port and write this information to
`"$HOME/.emacs-rpc-server-port"`.

<b>In the Client:</b> 

Read the server's port from `"$HOME/.emacs-rpc-server-port"`. Send a POST
request to `localhost:<port>` with the JSON-RPC 2.0 request encoded in the body.

```python
import requests

;; This is just an example
rpc_call = {
    "jsonrpc": "2.0"
    "method": "insert",
    "params": ["This is some text we want to insert"],
    "id": 23084
}
# This is just an example. In production, you would need much more error 
# checking. 
with open("$HOME/.emacs-rpc-server-port") as f:
    port = int(f.read()) 
response = requests.post("http://localhost:{}".format(port), json=rpc_call)
print(response.json())
```

### Example 2: Manual Setup, with Authentication

<b>In Emacs:</b> 

Start a server on a specific port:

```emacs-lisp
;; Functions have to be exposed before they can be invoked remotely.
(jrpc-expose-function 'insert)
;; Start a server on port 8000 with basic auth. Don't publish the port number.
(porthole-start-server
 :PORT 8000
 :USERNAME "my_username"
 :PASSWORD "my_password"
 :PUBLISH-PORT nil  ; The client will need to know the port number.
 )
```

<b>In the Client:</b> 

Send a POST request to port 8000 with the JSON-RPC 2.0 request encoded in the
body and the basic authentication credentials in the header.

In Python:

```python
import requests

;; This is just an example
rpc_call = {
    "jsonrpc": "2.0"
    "method": "insert",
    "params": ["This is some text we want to insert"],
    "id": 23084
}
# This is just an example. In production, you would need much more error 
# checking.
response = requests.post("http://localhost:8000", 
                         json=rpc_call,
                         auth=("my_username", "my_password"))
print(response.json())
```

## Publishing Functions

You can't call any function via RPC. The only functions that are allowed to be
called are those that have been manually exposed to RPC calls. This is also
handled by the underlying
[`json-rpc-server` package.](https://github.com/jcaw/json-rpc-server.el)

To expose a function, call `jrpc-expose-function`. Functions can be hidden with
`jrpc-hide-function`.

```emacs-lisp
;; We want to allow `insert' to be called via RPC. Expose the `insert' 
;; function to RPC calls.
(jrpc-expose-function 'insert)
;; We no longer want it to be callable. Hide `insert' from RPC calls.
(jrpc-hide-function 'insert)
```

See the [`json-rpc-server` package](https://github.com/jcaw/json-rpc-server.el)
for more information.

## Outside Localhost

Emacs' HTTP offerings servers only support Basic Authentication but they don't
support HTTPS. This means login credentials have to be sent over the network in
plain text. In other words, <b>don't allow connections to the server from
outside localhost.</b>

If you would like to allow access to the RPC server from the wider network, use
an HTTPS proxy. An Apache or Nginx proxy can forward external requests to the
local RPC server. The `emacs-web-server` manual has [more
detail](http://eschulte.github.io/emacs-web-server/tutorials/#sec-3).

## FAQ

- <b>Can I run multiple servers?</b> No. One per Emacs instance. Since they
  share the same server information file (they have to use a known name to be
  discoverable to clients), this is not recommended.
- <b>Is there support planned for other protocols than JSON-RPC?</b> Yes, but
  probably not for a while.
