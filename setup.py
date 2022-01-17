from distutils.core import setup
from setuptools import setup, find_packages
import os

bin_path = "./CCpy/bin/"
script_files = [bin_path + f for f in os.listdir(bin_path)]
script_files.sort()

install_requires = [
    'matplotlib'
    'pandas',
    'numpy',
    'pymatgen',
    'tables',
]
setup(
    name='CCpy',
    version='1.21',
    #packages=['CCpy','CCpy/ATAT','CCpy/ATK','CCpy/CASM','CCpy/Gaussian', 'CCpy/Gaussian/myGausssum',
    #          'CCpy/IKST','CCpy/Package','CCpy/Qchem','CCpy/Queue','CCpy/Tools','CCpy/VASP'],
    packages=find_packages(),
    package_data={'CCpy': ['Queue/queue_config.yaml', 'VASP/*yaml', 'VASP/vdw_kernel.bindat']},
    include_package_data=True,
    url='https://github.com/91bsjun/CCpy',
    license='',
    author='Byeongsun Jun',
    author_email='bjun915@gmail.com',
    description='',
    scripts=script_files
)
