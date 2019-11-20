#!/bin/bash

cd dotgen && stack build && stack exec dotgen-test
