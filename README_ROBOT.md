Robot Framework tests for DispatcherAgent

Prerequisites
- Activate your project's virtualenv: `.venv\Scripts\Activate.ps1` (PowerShell)
- Install requirements: `pip install -r requirements.txt`

Run tests (PowerShell)

```powershell
# Start server, run robot tests, then stop server
.\run_robot.ps1
```

Notes
- Tests use `RequestsLibrary` to exercise API and static frontend.
- If you prefer not to use the runner script, start the app manually:

```powershell
.venv\Scripts\python.exe -m uvicorn src.app.main:app --host 127.0.0.1 --port 8000
```

Then run Robot Framework directly:

```powershell
.venv\Scripts\python.exe -m robot.run tests/robot --outputdir results
```
