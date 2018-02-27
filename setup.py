from distutils.core import setup

install_requires = [
    'pandas',
    'numpy',
    'pymatgen',
]
setup(
    name='CCpy',
    version='1.20',
    packages=['CCpy/ATAT','CCpy/ATK','CCpy/bin','CCpy/CASM','CCpy/Gaussian',
              'CCpy/IKST','CCpy/Package','CCpy/Qchem','CCpy/Queue','CCpy/Tools','CCpy/VASP'],
    url='https://github.com/91bsjun/CCpy',
    license='',
    author='Byeongsun Jun',
    author_email='bjun915@gmail.com',
    description=''
)
