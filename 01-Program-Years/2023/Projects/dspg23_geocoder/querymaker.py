class querymaker:

    def __init__(self):
        pass


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
