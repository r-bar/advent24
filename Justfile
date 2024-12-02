YEAR := "2024"
BASE_URL := "https://adventofcode.com/{{YEAR}}"
COOKIE := env_var("COOKIE")
# the AoC day unlocks at midnight EST
TODAY := `printf '%-d' $(TZ=America/New_York date +%e)`

# show this list of commands
default:
  just --list

# create a new day folder
template-day num=TODAY: (mk-day-dir num) (template-readme num) (download-input num) (template-roc num) && (git-add num)
  touch $(just day-dir {{num}}){/answers.txt,/example.txt}


# Start part 1, template out a new day folder
part1 num=TODAY: (template-day num)


# Move on to part 2, update the readme with the part 2 text
part2 num=TODAY: (template-readme num) && (git-add num)


[private]
template-roc num:
  mkdir -p $(just day-dir {{num}})
  cp templates/template.roc $(just day-dir {{num}})/p1.roc
  cp templates/template.roc $(just day-dir {{num}})/p2.roc
  roc --version > $(just day-dir {{num}})/roc-version.txt


[private]
template-rust num:
  mkdir -p $(just day-dir {{num}})/src/bin
  touch $(just day-dir {{num}})/src/lib.rs
  cp templates/Cargo.toml $(just day-dir {{num}})/Cargo.toml
  sed -i "s/#\"$(just day-dir {{num}})\"/\"$(just day-dir {{num}})\"/" $(just day-dir {{num}})/Cargo.toml
  sed -i s/NAME/$(just day-dir {{num}})/ $(just day-dir {{num}})/Cargo.toml
  cp templates/template.rs $(just day-dir {{num}})/src/bin/d{{num}}p1.rs
  cp templates/template.rs $(just day-dir {{num}})/src/bin/d{{num}}p2.rs


[private]
git-add num:
  git add $(just day-dir {{num}})


# output the name of the day directory
# This is used to standardize naming and account for the lack of interpolation
[private]
day-dir num:
  @echo "day$(printf '%02d' {{num}})"


[private]
mk-day-dir num:
  mkdir -p $(just day-dir {{num}})


[private]
tmpdir:
  mkdir -p tmp


[private]
download-prompt num: tmpdir
  curl {{BASE_URL}}/day/{{num}} -H "Cookie: {{COOKIE}}" --fail > tmp/day{{num}}.html \
    || rm -f tmp/day{{num}}.html

# create a README.md in the day folder with the challenge text
template-readme num=TODAY: (download-prompt num)
  pup 'h2:contains("Day")' text{} < tmp/day{{num}}.html \
    | sed 's/^---/\#/' \
    | sed 's/ ---$$//' \
    > "$(just day-dir {{num}})/README.md"
  echo '## Part One' >> "$(just day-dir {{num}})/README.md"
  pup --pre .day-desc < tmp/day{{num}}.html \
    | pandoc -f html -t gfm \
    | sed '/# --- Day/d' \
    | sed '/Part Two/s/ ---//g' \
    | tee -a "$(just day-dir {{num}})/README.md"


# download and save the input for the given day in input.txt
download-input num=TODAY:
  curl {{BASE_URL}}/day/{{num}}/input -H "Cookie: {{COOKIE}}" --fail > "$(just day-dir {{num}})/input.txt" \
    || rm -f "$(just day-dir {{num}})/input.txt"


[private]
test num:
  #!/usr/bin/env bash
  set -euxo pipefail
  pwd
  echo {{COOKIE}}


# remove temporary files
clean:
  rm -r tmp


# post an answer to the advent of code website and save it to answers.txt
answer answer day=TODAY:
  curl --fail-with-body -X POST -H "Cookie: {{COOKIE}}" \
    -d "answer={{answer}}" \
    {{BASE_URL}}/day/{{day}}/answer
  echo {{answer}} >> $(just day-dir {{day}})/answers.txt
