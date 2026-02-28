*** Settings ***
Library    RequestsLibrary
Library    Collections

*** Variables ***
${BASE}    http://127.0.0.1:8000

*** Test Cases ***
Create Incident And Verify Assignment
    [Tags]    api
    Create Session    dispatcher    ${BASE}
    ${payload}=    Evaluate    {'node': 'n0_0', 'type': 'fire', 'severity': 3}
    ${resp}=    Post Request    dispatcher    /create_incident    json=${payload}
    Should Be Equal As Integers    ${resp.status_code}    200
    ${body}=    To Json    ${resp.content}
    Dictionary Should Contain Key    ${body}    assigned
    Dictionary Should Contain Key    ${body}    explanation

Get Graph And State
    [Tags]    api
    Create Session    dispatcher    ${BASE}
    ${g}=    Get Request    dispatcher    /graph
    Should Be Equal As Integers    ${g.status_code}    200
    ${s}=    Get Request    dispatcher    /state
    Should Be Equal As Integers    ${s.status_code}    200
    ${json}=    To Json    ${s.content}
    Dictionary Should Contain Key    ${json}    units

*** Keywords ***
To Json
    [Arguments]    ${content}
    ${decoded}=    Convert To String    ${content}
    ${json}=    Evaluate    __import__('json').loads('''${decoded}''')
    [Return]    ${json}
