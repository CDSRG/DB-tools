#The reticulate package provides a comprehensive set of tools for interoperability between Python and R. The package includes facilities for:
#•Calling Python from R in a variety of ways including R Markdown, sourcing Python scripts, importing Python modules, and using Python interactively within an R session.
#•Translation between R and Python objects (for example, between R and Pandas data frames, or between R matrices and NumPy arrays).
#•Flexible binding to different versions of Python including virtual environments and Conda environments.

#Reticulate embeds a Python session within your R session, enabling seamless, high-performance interoperability.
#for more info, see https://datascienceplus.com/how-to-make-seaborn-pairplot-and-heatmap-in-r-write-python-in-r/

install.packages('reticulate')

#need Python
#importing required Python libraries/modules
sns <- import('seaborn')
plt <- import('matplotlib.pyplot')
pd <- import('pandas')

