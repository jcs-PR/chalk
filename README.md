[![Build Status](https://travis-ci.com/jcs-elpa/chalk.svg?branch=master)](https://travis-ci.com/jcs-elpa/chalk)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# chalk
> Customize text easily with log.

<p align="center">
  <img src="./etc/demo1.png"/>
</p>

This package brings propertize string a bit easier for common usage.

## Usage

You can propertize string easily by supported API.

```el
(chalk-red "This is red")  ; Make string color red
```

Hence you can work with nested like this below.

```el
(message "%s %s" (chalk-red "red") (chalk-blue "blue"))
```

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
