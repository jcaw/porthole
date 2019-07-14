#!/usr/bin/env python3

"""This is a Python module designed to test external calls to the HTTP server.

It is designed to leverage Python's nosetests.

Do not run these tests directly. Use the provided elisp file,
`porthole-test-remote-calls.el`. It will set up a server instance and run these
tests against it.

"""


from nose.tools import eq_
from multiprocessing import Lock
import requests
import os
import platform
import json


TEST_SERVER_NAME = "external-tests-server"
PROTOCOL = "http://"
TEST_HOST = "localhost"

# Get the temp dir to use, depending on the platform.
system = platform.system()
if system.lower() == "windows":
    temp_dir = os.environ("TEMP")
elif system.lower() == "linux":
    temp_dir = os.environ.get("XDG_RUNTIME_DIR") or os.environ.get("HOME")
    if not temp_dir:
        raise IOError(
            "Neither $XDG_RUNTIME_DIR or $HOME could be read. "
            "Cannot automatically query server information on "
            "this Linux system."
        )
elif system.lower() == "mac":
    raise ValueError("Cannot test on Mac yet")
else:
    # TODO: swap to oserror?
    raise ValueError('Cannot test on this system: "{}"'.format(platform.system()))


SESSION_INFO_FILENAME = os.path.join(
    temp_dir, "emacs-porthole", TEST_SERVER_NAME, "session.json"
)


with open(SESSION_INFO_FILENAME) as f:
    SESSION_INFO = json.load(f)


print("Session info: ", SESSION_INFO)


TEST_PORT = SESSION_INFO["port"]
USERNAME = SESSION_INFO["username"]
PASSWORD = SESSION_INFO["password"]

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


# Boilerplate request. This is a valid JSON-RPC request - we can use it to test
# many aspects of the HTTP server.
SUM_REQUEST = _construct_json_rpc_request(method="+", params=[1, 2, 3])


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


def _assert_response_code(response, code):
    """Ensure the response has a specific error code.

    If not, the error message will be informative.

    """
    assert (
        response.status_code == code
    ), "Wrong response status code. {} != {}. Response text:\n{}".format(
        response.status_code, code, response.text
    )


def _assert_text_html(response):
    """Assert that the content is \"text/html\"."""
    assert "text/html" in _get_content_type(response), response.headers


def _assert_authentication_required(response):
    """Assert that the response to an unauthenticated request is correct.

    This checks that, when no authentication is given, the response has the
    right structure and content.

    We want to make sure nothing is revealed if the authentication fails. The
    client should not even know that they're querying an RPC server. This is
    why we don't reply with a JSON-RPC response.

    """
    _assert_response_code(response, 401)
    _assert_text_html(response)
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
    _assert_response_code(response, 401)
    _assert_text_html(response)
    # The content should reveal nothing.
    assert response.text.lower().strip() == "invalid credentials", response.text


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
    response = requests.post(TEST_ADDRESS, json=SUM_REQUEST, auth=(USERNAME, PASSWORD))
    _assert_response_code(response, 200)
    rpc_response = _extract_response_json(response)
    assert not "error" in rpc_response, rpc_response
    assert "result" in rpc_response, rpc_response
    result = rpc_response["result"]
    assert result == 6, rpc_response


def test_no_authentication():
    """Test sending a request with no authentication info."""
    # This should be a valid JSON-RPC 2.0 request.
    response = requests.post(TEST_ADDRESS, json=SUM_REQUEST)
    _assert_authentication_required(response)


def test_wrong_auth_method_digest():
    # `porthole` only supports Basic Authentication at the moment.
    # Passing the wrong authentication should trigger a 401 response.
    auth = requests.auth.HTTPDigestAuth(USERNAME, PASSWORD)
    response = requests.post(TEST_ADDRESS, json=SUM_REQUEST, auth=auth)
    print(response.request.headers)
    print(response.request.body)
    _assert_authentication_required(response)


def test_bad_authentication():
    """Test sending a request with invalid auth info."""
    # This should be a valid JSON-RPC 2.0 request.
    response = requests.post(
        TEST_ADDRESS, json=SUM_REQUEST, auth=("wrong" + USERNAME, "wrong" + PASSWORD)
    )
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
    # The important part for this test is that the *header* isn't JSON.
    # Requests will generate a different header from this content.
    request = "This is just some text content"
    response = requests.post(TEST_ADDRESS, data=request, auth=(USERNAME, PASSWORD))
    # TODO: What do we want to do here? Should something like this return an
    # HTTP error, or a JSON error?
    _assert_response_code(response, 400)
