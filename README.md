# software-foundations

A program for generating PDFs of the [Software Foundations](https://softwarefoundations.cis.upenn.edu/) books.

## Dependencies

- wget
- xfonts-75dpi
- [wkhtmltopdf](https://wkhtmltopdf.org/downloads.html)
- texlive-extra-utils
- [stack](https://docs.haskellstack.org/en/stable/README/)

## Clone and install

```
git clone https://github.com/onelharrison/software-foundations.git
cd software-foundations/
stack install
```

## Configure PATH

Ensure `~/.local/bin` is in your PATH

## Run program

```
software-foundations --help
software-foundations lf plf vfa qc
```

## Notes
* Inspired by [mk-software-foundations](https://github.com/superfunc/mk-software-foundations)
* All copyright and credit for these books belong to their respective authors
* For license see [LICENSE](LICENSE)
