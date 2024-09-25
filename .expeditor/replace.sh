#!/bin/bash

tmp_file_name=""

check_hart() {
    formated=` echo $1 | awk -F'-' -v b=2 -v e=7 '{printf "%s %s ", $(NF-2), $(NF-3); for (i=b;i<= (NF - 4);i++) printf "%s%s", $i, (i< (NF -4) ? "-" : "\n")}'`
    IFS=' ' read -r timestamp version component <<< "${formated}"
    echo "timestamp $timestamp, version $version, component $component"
    component1=`echo $component | tr -d "_\-\""`
    export ${component1}="$component/$version/$timestamp"
}

escape_char() {
  echo $1 | sed 's/\([-]\)/\\\1/g'
}

make_tmp() {
  count=0
  while [ -f "tmp_$count" ]
  do
    count=`expr $count + 1`
  done
  tmp_file_name="tmp_$count"
}

for hart in `ls results/*.hart`
do
  check_hart $hart
done

for component in `echo "automate-cs-bookshelf automate-cs-nginx automate-cs-oc-bifrost automate-cs-oc-erchef automate-cs-ocid"`
do
  plan_file="components/$component/habitat/plan.sh"
  echo "Processing $plan_file"
  make_tmp
  tmp_file=${tmp_file_name}
  cp $plan_file "$tmp_file"
  sed -n '/pkg_deps/,/)/p' < "$plan_file" | grep "/" |
  while read dep 
  do
    echo "processing $dep in $plan_file"
    dep_component=`echo $dep | awk -F'/' '{print $2}' | tr -d "_\-\""`
    echo "dep_component = ${dep_component}, value = ${!dep_component}"
    if [[ -n "${!dep_component}" ]]
    then
      IFS='/' read -r CScomponent version timestamp <<< "${!dep_component}"
      echo "timestamp $timestamp, version $version, component $CScomponent"
      CScomponent1=`escape_char $CScomponent`
      echo "sed \"/\/$CScomponent1/ s|\/$CScomponent1.*|$CScomponent1/$version/$timestamp|\""
      sed -i "/pkg_deps/,/)/ s|\/$CScomponent1.*|/$CScomponent1/$version/$timestamp\"|" "${tmp_file}"
    fi
  done
  cp $tmp_file $plan_file
  rm $tmp_file "${tmp_file}tmp"
  echo "=================================="
  echo "file content after replace"
  cat $plan_file
  echo "=================================="
  echo "results dir content"
  ls results/*.hart
  echo "=================================="
done

exit 0