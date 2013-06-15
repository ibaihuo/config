# Find python file
alias pyfind='find . -name "*.py"'

# Remove python compiled byte-code in either current directory or in a
# list of specified directories
function pyclean() {
    ZSH_PYCLEAN_PLACES=${*:-'.'}
    find ${ZSH_PYCLEAN_PLACES} -type f -name "*.py[co]" -delete
}


# for python lib source code
function pycd () {
		 cd $(dirname $(python -c "print __import__('$1').__file__"));

		 }

# Grep among .py files
alias pygrep='grep --include="*.py"'
