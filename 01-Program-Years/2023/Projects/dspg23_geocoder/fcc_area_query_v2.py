import numpy as np
import os
import pandas as pd
from tqdm import tqdm
import argparse
import pandas as pd
import requests
from tqdm import tqdm
import warnings
import logging
import time

# Constants
API_ENDPOINT =  "https://geocoding.geo.census.gov/geocoder/locations/addressbatch"
IP_CHUNK_NAME = "fcc_chunk_{i}.csv"
OP_CHUNK_NAME = "{chunk_name}_geocoded.csv"


# Configs
warnings.filterwarnings("ignore")



def parse_arguments():
    """
    DEFINITION
    --Parameters--
    --Raises--
    """
    parser = argparse.ArgumentParser(
        description="Given a csv, create an ouput directory with batched fcc area geocoding queries. Assumes that the csv is already formatted to fcc area api standards. Note if the force flag is false, the process will continue where it left off as long as the ouput_dir is the same."
    )
    parser.add_argument(
        "-i",
        "--input_file",
        type=str,
        help="The input csv",
        required=True,
    )
    parser.add_argument(
        "-o",
        "--output_dir",
        type=str,
        help="The output directory",
        required=True,
    )
    parser.add_argument(
        "-v",
        "--verbose",
        help="Show debugging outputs",
        action=argparse.BooleanOptionalAction,
    )
    parser.add_argument(
        "-f",
        "--force",
        action=argparse.BooleanOptionalAction,
        help="Whether or not to override the output directory files",
        required=False,
        default=False,
    )

    args = parser.parse_args()

    return args




def chunk_files(input_file, output_dir):
    """
    There is a max api row limit of 10,000. Therefore, we need to split the data frames apart if we are requesting anything longer
    --Parameters--
    --Raises--
    """
    df = pd.read_csv(input_file)
    dfs = np.array_split(df, (len(df) / 10000) + 1)
    chunk_names = []
    for i in range(len(dfs)):
        save_name = os.path.join(
            output_dir,
            IP_CHUNK_NAME.format(
                i=str(i).zfill(int(len(str(len(dfs)))))
            ),  # zfill by the lenght of theh string of the length of the value
        )
        try:
            dfs[i].to_csv(save_name)
        except:
            pass
        logging.debug(os.path.isfile(save_name))
        if os.path.isfile(save_name):
            chunk_names.append(save_name)
    logging.debug(len(chunk_names)) 
    return chunk_names


def batch_geocode(chunk_names, force):
    pbar = tqdm(chunk_names)
    for chunk in pbar:
        save_name = OP_CHUNK_NAME.format(
            chunk_name=chunk.split(".csv")[0]
        )
        logging.debug(save_name)
        pbar.set_description(save_name)
        if not force and os.path.isfile(
            save_name
        ):  # skip if already downloaded, and force flag is False
            continue
        time.sleep(15)  # Wait after you check the file does not exist in the directory
        files = {
            "addressFile": open(chunk, "r"),
            "benchmark": (None, "2020"),
        }

        if not force and os.path.isfile(
            save_name
        ):  # if not force and the file is already read
            continue

        response = requests.post(
           API_ENDPOINT,
            files=files,
        )
        logging.debug("Saving to: %s" % save_name)
        with open(save_name, "wb") as f:
            f.write(response.content)

        logging.debug("Fixing malformed headers")
        with open(save_name, "r", encoding="utf-8") as file:
            data = file.readlines()

        data[0] = "index,input,match,non_exact,street,coordinate,tiger,lr\n"

        with open(save_name, "w", encoding="utf-8") as file:
            file.writelines(data)


def cleanup(output_dir):   
    logging.info("Deleting all non_geocoded files")
    for file in os.listdir(output_dir):
        if "_geocoded" not in file:
            os.remove(os.path.join(output_dir, file))



def batch_geocode_util(input_file, output_dir, force):
    """DocString to document the function
    https://realpython.com/documenting-python-code/
    """
    chunk_names = chunk_files(input_file, output_dir)
    batch_geocode(chunk_names, force)
    cleanup(output_dir)





if __name__ == "__main__":
    """
    Explain what main function does
    """
    args = parse_arguments()
    log_level = logging.INFO
    if args.verbose:
        log_level = logging.DEBUG

    logging.basicConfig(format="%(levelname)s: %(message)s", level=log_level)

    if not os.path.isdir(args.output_dir):
        os.mkdir(args.output_dir)

    batch_geocode_util(args.input_file, args.output_dir, args.force)




"""
What I have done?

1. Removed unused imports
2. Added DocStrings to describe the function
3. ParseArgs to a different function
4. Constants and Configs above everything
5. Chunked the main function and abstracted other methods
6. Renamed functions and snake_cased everything

"""