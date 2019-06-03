#!/usr/bin/env python3

"""This is a Python module designed to test external calls to the HTTP server.

It is designed to leverage Python's nosetests.

Do not run these tests directly. Use the provided elisp file,
`test-http-rpc-server.el`. It contains a function which will set up an instance
of the server and run these tests against it.

"""


from nose.tools import eq_
from multiprocessing import Lock
import requests
import os
import platform

USERNAME = "test_username"
PASSWORD = "test_password"

PROTOCOL = "http://"
TEST_HOST = "localhost"

system = platform.system()
if system.lower() == "windows":
    PORT_FILENAME = os.path.join(
        (
            os.path.expandvars("%userprofile%")
            or (os.path.expandvars("%homedrive%") + os.path.expandvars("%homepath%"))
        ),
        ".emacs-rpc-server-port",
    )
elif system.lower() in ["linux", "mac"]:
    PORT_FILENAME = os.path.expanduser("~/.emacs-rpc-server-port")
else:
    raise ValueError('Cannot test on this system: "{}"'.format(platform.system()))


def _read_port_from_file():
    """Read the server's current port number from the port file.

    :returns: the server's current port number
    :rtype: int

    """
    with open(PORT_FILENAME) as f:
        port_as_string = f.read()
    port_as_string = port_as_string.strip()
    try:
        return int(port_as_string)
    except:
        raise ValueError(
            'Port file did not contain an integer. Was: "{}"'.format(port_as_string)
        )


TEST_PORT = _read_port_from_file()

TEST_ADDRESS = "{protocol}{host}:{port}".format(
    protocol=PROTOCOL, host=TEST_HOST, port=TEST_PORT
)

id_lock = Lock()
last_id = 0


def _generate_id():
    """Generate a unique ID for the request."""
    global last_id, id_lock
    id_lock.acquire()
    try:
        last_id += 1
        return last_id
    finally:
        id_lock.release()


def _construct_json_rpc_request(method, params=None):
    """Create a new JSON-RPC request.

    Returns the raw request - not yet encoded into JSON."""
    request = {"jsonrpc": "2.0", "method": method, "id": _generate_id()}
    # Only add the `params` parameter if there are params.
    if params:
        request["params"] = params
    return request


def _get_content_type(response):
    return response.headers["Content-type"]


def _extract_response_json(response):
    """Extract the JSON from a response, with informative errors."""
    assert "application/json" in _get_content_type(response), "{}\n{}".format(
        response.headers, response.text
    )
    try:
        rpc_response = response.json()
    except:
        raise ValueError(
            "Response not JSON. Headers: \n{}\nBody:\n{}".format(
                response.headers, response.text
            )
        )
    return rpc_response


def _assert_plaintext(response):
    assert "text/plain" in _get_content_type(response), response.headers


def _assert_authentication_required(response):
    """Assert that the response to an unauthenticated request is correct.

    This checks that, when no authentication is given, the response has the
    right structure and content.

    We want to make sure nothing is revealed if the authentication fails. The
    client should not even know that they're querying an RPC server. This is
    why we don't reply with a JSON-RPC response.

    """
    _assert_plaintext(response)
    # Response should indicate that authentication is required.
    eq_(response.status_code, 401)
    # The content should reveal nothing.
    assert response.text.lower().strip() == "authentication required", response.text


def _assert_invalid_credentials(response):
    """Assert that the response to invalid credentials is correct.

    This checks that, when authentication is given but it is bad, the response
    has the right structure and content.

    We want to make sure nothing is revealed if the authentication fails. The
    client should not even know that they're querying an RPC server. This is
    why we don't reply with a JSON-RPC response.

    """
    _assert_plaintext(response)
    eq_(response.status_code, 403)
    # The content should reveal nothing.
    assert response.text.lower().strip() == "invalid credentials", response.text


# TODO: Verify JSON response structure.
# TODO: Verify HTTP response codes.


def _extract_rpc_error(rpc_response):
    """Extract the error from a JSON-RPC 2.0 response.

    Also checks the structure of the error, and throws out errors with the full
    `rpc_response` if something is amiss.

    """
    assert "error" in rpc_response, rpc_response
    error = rpc_response["error"]
    assert "code" in error, rpc_response
    assert isinstance(error["code"], int)
    assert "message" in error, rpc_response
    assert isinstance(error["message"], str)
    assert "data" in error, rpc_response
    data = error["data"]
    assert data is None or isinstance(data, dict), rpc_response
    return error


def test_acceptable_call():
    """Test a fully acceptable RPC call."""
    request = _construct_json_rpc_request(method="+", params=[1, 2, 3])
    response = requests.post(TEST_ADDRESS, json=request, auth=(USERNAME, PASSWORD))
    rpc_response = _extract_response_json(response)
    assert not "error" in rpc_response, rpc_response
    assert "result" in rpc_response, rpc_response
    result = rpc_response["result"]
    assert result == 6, rpc_response


def test_no_authentication():
    """Test sending a request with no authentication info."""
    # This should be a valid JSON-RPC 2.0 request.
    request = _construct_json_rpc_request(method="+", params=[1, 2, 3])
    response = requests.post(TEST_ADDRESS, json=request)
    _assert_authentication_required(response)


def test_bad_authentication():
    """Test sending a request with invalid auth info."""
    # This should be a valid JSON-RPC 2.0 request.
    request = _construct_json_rpc_request(method="+", params=[1, 2, 3])
    response = requests.post(
        TEST_ADDRESS, json=request, auth=("wrong" + USERNAME, "wrong" + PASSWORD)
    )
    # `emacs-web-sever` gives 403 status codes to all invalid authentication
    # credentials (rather than just those that are valid, but have no access to
    # the service). This doesn't really matter for our purposes. Target 403 in
    # the response.
    _assert_invalid_credentials(response)


def test_malformed_json():
    """Test sending a request malformed JSON content."""
    # We only need to include one test that tests the functionality of the RPC
    # layer. We're just testing that the functionality is exposed correctly.
    # The rest of that layer should be tested in its `ert` tests.

    # Let's use an empty request.
    request = {}
    response = requests.post(TEST_ADDRESS, json=request, auth=(USERNAME, PASSWORD))
    rpc_response = _extract_response_json(response)
    error = _extract_rpc_error(rpc_response)
    # This error code corresponds to "invalid request" in the JSON-RPC 2.0
    # specification.
    assert error["code"] == -32600, rpc_response


def test_not_json_content():
    """Test sending a request with content that isn't JSON."""
    request = "This is just some text content"
    response = requests.post(TEST_ADDRESS, data=request, auth=(USERNAME, PASSWORD))
    # TODO: What do we want to do here? Should something like this return an
    # HTTP error, or a JSON error?
    eq_(response.status_code, 400)
