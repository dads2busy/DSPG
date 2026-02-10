# dspg23_geocoder

This is part of Alan's training in geocoding and collaborative development.

## Proposed structure
4:00 pm July 24, 2023. Steve

Our program should consist of five modules:
<ol type="1">
  <li>file_reader module</li>
    This module reads files of different formats, including .xlsx, .csv, .txt, etc. It also handles errors related to the file path.
  <li>address_lookup module (optional)</li>
    This optional module should be able to find which column contains the street address, even if the user does not specify one.
  <li>query_maker module</li>
    This module should contain multiple classes, each responding to querying a particular API. It should also handle problems specific to each API, such as error codes, rate limits, and corrupted URLs.
  <li>result_verifier module</li>
    This module verifies the result returned from the queries and handles the logic of re-trying a lower-priority API on failed ones.
  <li>output module</li>
    This module organizes the data in user-designated formats and writes the output to the specified directory.
</ol>

The deliverable will be a command-line program with the following parameters:
<ol>
  <li>input_directory</li>
  <li>output_directory</li>
  <li> <i>name of address column (optional)</i> </li>
  <li> <i>output format (optional)</i> </li>
  <li> <i>api preference list (optional)</i> </li>
</ol>
