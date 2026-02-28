*** Settings ***
Library    Process

*** Variables ***
${PY}    ${CURDIR}\..\..\.venv\Scripts\python.exe
${DELAY}    15

*** Test Cases ***
Visual Playwright Click (external)
    [Tags]    visual
    Run Process    ${PY}    ${CURDIR}\visual_demo.py    ${DELAY}
    Should Be Equal As Integers    ${rc}    0
