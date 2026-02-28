*** Settings ***
Library    RequestsLibrary
Library    Collections

*** Variables ***
${BASE}    http://127.0.0.1:8000

*** Test Cases ***
Root Page Contains Frontend
    [Tags]    ui
    Create Session    dispatcher    ${BASE}
    ${r}=    Get Request    dispatcher    /
    Should Be Equal As Integers    ${r.status_code}    200
    ${text}=    Convert To String    ${r.content}
    Should Contain    ${text}    Emergency Dispatcher Simulation

Create Incident Via UI Endpoint
    [Tags]    ui
    Create Session    dispatcher    ${BASE}
    ${payload}=    Evaluate    {'node': 'n0_0', 'type': 'medical', 'severity': 2}
    ${resp}=    Post Request    dispatcher    /create_incident    json=${payload}
    Should Be Equal As Integers    ${resp.status_code}    200
    ${body}=    To Json    ${resp.content}
    Dictionary Should Contain Key    ${body}    assigned

*** Keywords ***
To Json
    [Arguments]    ${content}
    ${decoded}=    Convert To String    ${content}
    ${json}=    Evaluate    __import__('json').loads('''${decoded}''')
    [Return]    ${json}
