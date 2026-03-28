#!/bin/bash
# CocoScript installer - handles dependencies, cloning, building, and installation

set -e

COCOVM_DIR="$HOME/.cocovm"
REPO_URL="https://github.com/dwenginw-tech/cocoscriptomal.git"
INSTALL_DIR="$COCOVM_DIR/cocoscript"

echo "=== CocoScript Installer ==="
echo ""

# Check if running on Linux
if [[ "$OSTYPE" != "linux-gnu"* ]]; then
    echo "Error: This installer is for Linux only"
    echo "For Windows, download the installer from the releases page"
    exit 1
fi

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check for required dependencies
echo "Checking dependencies..."

MISSING_DEPS=()

if ! command_exists git; then
    MISSING_DEPS+=("git")
fi

if ! command_exists opam; then
    MISSING_DEPS+=("opam")
fi

if ! command_exists gcc; then
    MISSING_DEPS+=("gcc")
fi

if ! command_exists nasm; then
    MISSING_DEPS+=("nasm")
fi

if [ ${#MISSING_DEPS[@]} -ne 0 ]; then
    echo ""
    echo "Missing dependencies: ${MISSING_DEPS[*]}"
    echo ""
    echo "Please install them first:"
    echo "  Ubuntu/Debian: sudo apt install git opam gcc nasm"
    echo "  Fedora: sudo dnf install git opam gcc nasm"
    echo "  Arch: sudo pacman -S git opam gcc nasm"
    echo ""
    echo "After installing dependencies, run this script again."
    exit 1
fi

echo "All dependencies found!"
echo ""

# Create cocovm directory
echo "Creating $COCOVM_DIR..."
mkdir -p "$COCOVM_DIR"

# Clone or update repository
if [ -d "$INSTALL_DIR" ]; then
    echo "CocoScript already installed. Updating..."
    cd "$INSTALL_DIR"
    git pull
else
    echo "Cloning CocoScript repository..."
    git clone "$REPO_URL" "$INSTALL_DIR"
    cd "$INSTALL_DIR"
fi

echo ""
echo "Building CocoScript..."
opam exec -- dune build

# Install the binary
echo "Installing cocoscript binary..."
mkdir -p "$COCOVM_DIR/bin"
cp "_build/default/src/driver/main.exe" "$COCOVM_DIR/bin/cocoscript"
chmod +x "$COCOVM_DIR/bin/cocoscript"

# Install standard library
echo "Installing standard library..."
mkdir -p "$COCOVM_DIR/lib"
cp -r src/lib/* "$COCOVM_DIR/lib/"

# Create cocovm command
echo "Creating cocovm command..."
cat > "$COCOVM_DIR/bin/cocovm" << 'EOF'
#!/bin/bash
# CocoScript Version Manager

COCOVM_DIR="$HOME/.cocovm"
INSTALL_DIR="$COCOVM_DIR/cocoscript"
REPO_URL="https://github.com/dwenginw-tech/cocoscriptomal.git"

case "$1" in
    install)
        if [ -d "$INSTALL_DIR" ]; then
            echo "CocoScript is already installed. Use 'cocovm update' to update."
        else
            echo "Installing CocoScript..."
            git clone "$REPO_URL" "$INSTALL_DIR"
            cd "$INSTALL_DIR"
            opam exec -- dune build
            cp "_build/default/src/driver/main.exe" "$COCOVM_DIR/bin/cocoscript"
            chmod +x "$COCOVM_DIR/bin/cocoscript"
            mkdir -p "$COCOVM_DIR/lib"
            cp -r src/lib/* "$COCOVM_DIR/lib/"
            echo "CocoScript installed successfully!"
        fi
        ;;
    update)
        if [ ! -d "$INSTALL_DIR" ]; then
            echo "CocoScript is not installed. Use 'cocovm install' first."
            exit 1
        fi
        echo "Updating CocoScript..."
        cd "$INSTALL_DIR"
        git pull
        opam exec -- dune clean
        opam exec -- dune build
        cp "_build/default/src/driver/main.exe" "$COCOVM_DIR/bin/cocoscript"
        chmod +x "$COCOVM_DIR/bin/cocoscript"
        cp -r src/lib/* "$COCOVM_DIR/lib/"
        echo "CocoScript updated successfully!"
        ;;
    uninstall)
        echo "Uninstalling CocoScript..."
        rm -rf "$COCOVM_DIR"
        echo "CocoScript uninstalled. Please remove the PATH entry from your shell config."
        ;;
    version)
        if [ -x "$COCOVM_DIR/bin/cocoscript" ]; then
            cd "$INSTALL_DIR"
            echo "CocoScript $(git describe --tags --always)"
        else
            echo "CocoScript is not installed"
        fi
        ;;
    *)
        echo "CocoScript Version Manager"
        echo ""
        echo "Usage: cocovm <command>"
        echo ""
        echo "Commands:"
        echo "  install    - Install CocoScript"
        echo "  update     - Update CocoScript to latest version"
        echo "  uninstall  - Remove CocoScript"
        echo "  version    - Show installed version"
        ;;
esac
EOF

chmod +x "$COCOVM_DIR/bin/cocovm"

# Add to PATH if not already there
SHELL_CONFIG=""
if [ -n "$BASH_VERSION" ]; then
    SHELL_CONFIG="$HOME/.bashrc"
elif [ -n "$ZSH_VERSION" ]; then
    SHELL_CONFIG="$HOME/.zshrc"
fi

if [ -n "$SHELL_CONFIG" ]; then
    if ! grep -q "COCOVM_DIR" "$SHELL_CONFIG"; then
        echo "" >> "$SHELL_CONFIG"
        echo "# CocoScript" >> "$SHELL_CONFIG"
        echo "export PATH=\"\$HOME/.cocovm/bin:\$PATH\"" >> "$SHELL_CONFIG"
        echo "" >> "$SHELL_CONFIG"
        echo "Added CocoScript to PATH in $SHELL_CONFIG"
        echo "Run: source $SHELL_CONFIG"
    fi
fi

echo ""
echo "=== Installation Complete! ==="
echo ""
echo "CocoScript has been installed to: $COCOVM_DIR"
echo ""
echo "To use CocoScript, either:"
echo "  1. Restart your terminal, or"
echo "  2. Run: source $SHELL_CONFIG"
echo ""
echo "Then you can use:"
echo "  cocoscript <file.coco>  - Compile and run a CocoScript file"
echo "  cocovm update           - Update CocoScript"
echo "  cocovm uninstall        - Remove CocoScript"
echo ""
