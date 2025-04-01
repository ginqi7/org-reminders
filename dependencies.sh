solve_dependency () {
    url=$1
    echo "Installing dependency: ${url}"
    mkdir -p ~/.emacs.d/lisp;
    name=$(echo ${url} | awk -F'/' '{print $NF}');
    if [ ! -d ~/.emacs.d/lisp/${name} ]; then
	git clone ${url} ~/.emacs.d/lisp/${name};
    else
	echo "${name} already exists, skipping...";
    fi
    echo "Dependencies installed installed successfully."
}

parse_dependencies() {
    while IFS= read -r line; do
        solve_dependency $line
    done < $PWD/dependencies.txt
}
parse_dependencies
