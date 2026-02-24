import sys
from pathlib import Path

try:
    from PyPDF2 import PdfReader
except Exception:
    print('PyPDF2 not installed', file=sys.stderr)
    raise

if len(sys.argv) < 3:
    print('Usage: python extract_pdf_text.py <pdf_path> <out_txt_path>', file=sys.stderr)
    sys.exit(2)

pdf_path = Path(sys.argv[1])
out_path = Path(sys.argv[2])

reader = PdfReader(str(pdf_path))
with out_path.open('w', encoding='utf-8') as f:
    for i, page in enumerate(reader.pages, start=1):
        try:
            text = page.extract_text() or ''
        except Exception as e:
            text = ''
        f.write(f"\n\n--- Page {i} ---\n\n")
        f.write(text)

print(f'Extracted text to {out_path}')
