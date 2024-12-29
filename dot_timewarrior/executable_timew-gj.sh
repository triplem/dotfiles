#!/bin/bash
#
# partly stolen from toki
# https://github.com/cheap-glitch/toki/blob/main/src/toki
# 

args=()
for arg in "${@}"; do

  if [[ "${arg,,}" == ":businessyear" ]]; then
    year=$(date +%Y)
    nextyear=$((year+1))
    args+=("$year-04-01")
    args+=("-")
    args+=("$nextyear-03-31")
    continue
  fi

  # Convert `<x>m` to `<x>minutes`
  if [[ "${arg,,}" =~ ^[0-9]+(\.[0-9]+)?m$ ]]; then
    args+=("${arg,,}inutes")
    continue
  fi

  # Parse `*h*m*s` notation
  if [[ "${arg,,}" =~ ^[0-9]+h[0-9]+m?|([0-9]+h)?[0-9]+m[0-9]+s?$ ]]; then
    duration="${arg%%[mMsS]}"
    if [[ "${duration,,}" == *m* ]]; then
      args+=("PT${duration^^}S")
    else
      args+=("PT${duration^^}M")
    fi
    continue
  fi 

  args+=("${arg}")
done

echo timew "${args[@]}"
timew "${args[@]}"
