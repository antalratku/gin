import setuptools

with open('README.md', 'r') as fh:
    long_description = fh.read()

setuptools.setup(
    name='gin',
    version='0.1',
    author='Antal Ratku',
    author_email='antal.ratku@hotmail.com',
    description='A GPU accelerated numerical integration library.',
    long_description=long_description,
    url='https://github.com/antalratku/gin',
    packages=setuptools.find_packages(),
    classifiers=[
        'Programming Language :: Python :: 3',
        'License :: OSI Approved :: MIT License',
        'Operating System :: Microsoft Windows 10'
    ],
    python_requires='>=3.8.5'
)