# Advent of Code 2024

https://adventofcode.com/2024

This year's goal is to gain familiarity with the Roc programming language.

## Add a new challenge folder

```
export COOKIE=session=53616c7...
just part1
```

This command will automatically create a `README.md` with the prompt(s). 
The challenge input will be saved to `input.txt`.

This command communicates with the Advent of Code servers to fetch this data.
**The given day must be live** before the folder will be able to be templated.
The download also requires your session cookie to fetch your personalized data.

This cookie is fairly long lived and can be extracted from the `Cookie` header
for any request to adventofcode.com after you are logged in. The set the
`COOKIE` environment variable with this session value

When ready for the next section you can update the readme with `just part2`.

Most of the `just` commands accept the day number as the first argument. This
only needs to be specified if the command is not run on the same day as the
challenge.

## Run the code (day 5, part 1)
```
cd day05
roc p1.roc -- input.txt
```

## Requirements
* [Roc](https://roc-lang.org/) version:
```
roc nightly pre-release, built from commit d72da8e on Fr 29 Nov 2024 09:11:57 UTC
```
* [just](https://github.com/casey/just)
* curl
* [pup](https://github.com/ericchiang/pup)
* GNU sed
* pandoc

