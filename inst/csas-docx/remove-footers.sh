#!/bin/bash

# Script to completely remove footers from a Word document
# Usage: ./remove-footers.sh input.docx [output.docx]

if [ $# -lt 1 ]; then
    echo "Usage: $0 input.docx [output.docx]"
    exit 1
fi

INPUT="$1"
OUTPUT="${2:-$INPUT}"
TEMP_DIR=$(mktemp -d)

echo "Extracting $INPUT..."
unzip -q "$INPUT" -d "$TEMP_DIR"

# Remove footer files
echo "Removing footer XML files..."
rm -f "$TEMP_DIR/word/footer"*.xml

# Remove footer relationships from document.xml.rels
echo "Cleaning relationships..."
if [ -f "$TEMP_DIR/word/_rels/document.xml.rels" ]; then
    perl -i -pe 's/<Relationship[^>]*Type="[^"]*\/footer"[^>]*\/>\n?//g' "$TEMP_DIR/word/_rels/document.xml.rels"
fi

# Remove footer references from document.xml (section properties)
echo "Cleaning document.xml..."
if [ -f "$TEMP_DIR/word/document.xml" ]; then
    perl -i -pe 's/<w:footerReference[^>]*\/>\n?//g' "$TEMP_DIR/word/document.xml"
fi

# Remove footer content types from [Content_Types].xml
echo "Cleaning content types..."
if [ -f "$TEMP_DIR/[Content_Types].xml" ]; then
    perl -i -pe 's/<Override[^>]*\/footer[^>]*\/>\n?//g' "$TEMP_DIR/[Content_Types].xml"
fi

# Repackage the docx
echo "Repackaging document..."
cd "$TEMP_DIR"
TEMP_OUTPUT="$(mktemp).docx"
zip -q -r "$TEMP_OUTPUT" *
cd - > /dev/null

# Move to final location
mv "$TEMP_OUTPUT" "$OUTPUT"

# Cleanup
rm -rf "$TEMP_DIR"

echo "Done! Footers removed from $OUTPUT"
