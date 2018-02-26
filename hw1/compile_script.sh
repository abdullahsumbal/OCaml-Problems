#!/bin/bash

ocamlbuild "$1.native"
./"$1.native"
