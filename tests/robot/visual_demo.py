import sys
import time
from pathlib import Path

from playwright.sync_api import sync_playwright

BASE = "http://127.0.0.1:8000"
DELAY = 10
if len(sys.argv) > 1:
    try:
        DELAY = int(sys.argv[1])
    except Exception:
        pass

with sync_playwright() as p:
    browser = p.chromium.launch(headless=False)
    page = browser.new_page()
    page.goto(BASE)
    # inject prompt override
    page.evaluate("window._prompts=['fire','3']; window.prompt = function(){ return window._prompts.shift(); }")
    # wait for nodes
    page.wait_for_selector('svg circle.node', timeout=5000)
    # click first node
    page.eval_on_selector_all('svg circle.node', "(els) => els[0].dispatchEvent(new MouseEvent('click', {bubbles:true, cancelable:true}))")
    time.sleep(DELAY)
    browser.close()
