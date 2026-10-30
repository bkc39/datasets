packages=($(<"$project_root/scripts/packages.txt"))
core=${packages[0]}
dependents=("${packages[@]:1}")
families=("${dependents[@]}")
unset "families[${#families[@]}-1]"
