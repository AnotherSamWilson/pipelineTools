
# Pipeline Tools

Personal package of very useful funcitons used in modeling pipeline.
Features:

  - Encoding
      - Frequency  
      - Gaussian (center scaling)  
      - Dummy Variables (faster than caret\!)  
      - NA encoding  
      - Ordinal Encoding  
      - Uniform Encoding  
      - Rare Value Aggregation  
      - Flexible Box Cox Transformation  
  - ODBC Connection functions  
  - xgbfi Wrappers  
  - Fast and Easy data.table toCSV()  
  - Multi-Character Substitution

All encodings have associated ‘apply’ S3 methods, so the same object can
be applied to any applicable dataset.
