*** Settings ***
Library    Browser
Library    RequestsLibrary

*** Variables ***
${BASE}    http://127.0.0.1:8000
${VISUAL_DELAY}    10s

*** Test Cases ***
Open UI And Trigger Incident (visual)
    [Tags]    ui_playwright
    New Browser    headless=False
    New Page    ${BASE}/
    Sleep    1s
    # override prompt to return pre-defined answers in sequence
    Execute JavaScript    window._prompts = ['fire','3']; window.prompt = function(){ return window._prompts.shift(); }

    # wait for SVG nodes to be rendered
    Wait For Elements State    css=svg circle.node    visible    timeout=5s

    # simulate a click on the first node (index 0)
    Execute JavaScript    document.querySelectorAll('svg circle.node')[0].dispatchEvent(new MouseEvent('click', {bubbles:true, cancelable:true}));

    # optionally wait for a network request or observable change
    Sleep    0.5s
    Sleep    ${VISUAL_DELAY}
    Close Browser
