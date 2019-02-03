from datetime import datetime
from os import system
from os.path import join

import matplotlib.pyplot as plt
import numpy as np
from mpl_toolkits.basemap import Basemap

from _FileSearch import FileSearch
from _Settings import Settings
from colormap import Colormap
from satellite import _sate_param
from satellite.utils import (convert_time_format, get_multi_choices_template,
                             is_ascii, safe_input, safe_str2value,
                             unique_elements)

DEBUG = False

class Satima:

    def __init__(self, FileSearch):
        self.fid = FileSearch.choice
        self.settings = Settings().settings
        self.fileclass = FileSearch.fileclass[self.fid['type'].upper()](self.fid)
        self.channel_flag = self.fileclass.get_channel_info()
        self.common_imoptions = {'Common':[], 'Composite':[]}
        self.spec_imoptions = {'WV':{'channels':[],'colormaps':[]}, 'IR':{'channels':[],'colormaps':[]}}
        self.raw = {}
        self.run()