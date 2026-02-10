import os
import pandas as pd


class FileReader:
    path = None
    isPathFile = False
    isPathDir = False

    def __init__(self, input_dir: str):
        if self.is_path_valid(input_dir):
            self.path = input_dir
        else:
            raise ValueError("Input directory is not valid!")

    def is_path_valid(self, path):
        if os.path.exists(path):
            if os.path.isfile(path):
                self.isPathFile = True
            else:
                self.isPathDir = True
            return True
        else:
            return False

    def read(self, **kwargs):
        """
        Read dataframe from file. If path is specified, read the path, otherwise read
        self.path. If self.path is directory, read only the first file in directory.

        :param path: optional file path
        :param sep: optional alternative seperating character for .csv
        :param sheet_name: optional sheet name or index for Excel file
        :return: single dataframe, if the file is a csv or Excel sheet
        """
        def read_file(path, sep=',', sheet_name=0):
            df = None
            if path.lower().endswith([".txt", ".csv"]):
                df = pd.read_csv(path, sep=sep)
            elif path.lower().endswith([".xlsx", ".xls"]):
                try:
                    df = pd.read_excel(path, sheet_name=sheet_name)
                except Exception as e:
                    print(e)
            return df

        sep = kwargs.get("sep", default=',')
        sheet_name = kwargs.get("sheet_name", default=0)

        if "path" not in kwargs.keys():
            if self.isPathDir:
                file_list = [os.path.join(self.path, file) for file in os.listdir(self.path)]
                path = file_list[0]
            else:
                path = self.path
        else:
            path = kwargs.get("path")

        return read_file(path, sep=sep, sheet_name=sheet_name)

    def read_all(self, **kwargs):
        """
        Read all dataframes in the directory. Only works if self.path is a directory

        :return: list of pd.DataFrame
        """
        if not self.isPathDir:
            raise ValueError("Path is not directory, cannot call read_all")
        else:
            file_list = [os.path.join(self.path, file) for file in os.listdir(self.path)]
            results = [self.read(path=path) for path in file_list]
            return [result for result in results if result is not None]


