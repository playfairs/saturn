{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.CLI.Help (
    showHelp,
    showCommandHelp,
    showUsage,
    showExamples,
    HelpTopic (..),
) where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text

data HelpTopic
    = Overview
    | Init
    | Apply
    | Rollback
    | Validate
    | List
    | Diff
    | Doctor
    | Configuration
    | Modules
    | Troubleshooting
    deriving (Show, Eq)

showHelp :: Maybe HelpTopic -> Text
showHelp Nothing = showOverview
showHelp (Just topic) = case topic of
    Overview -> showOverview
    Init -> showInitHelp
    Apply -> showApplyHelp
    Rollback -> showRollbackHelp
    Validate -> showValidateHelp
    List -> showListHelp
    Diff -> showDiffHelp
    Doctor -> showDoctorHelp
    Configuration -> showConfigurationHelp
    Modules -> showModulesHelp
    Troubleshooting -> showTroubleshootingHelp

showOverview :: Text
showOverview =
    Text.unlines
        [ "mycfg - Declarative Configuration Manager"
        , ""
        , "A declarative configuration management system inspired by NixOS, Home Manager,"
        , "and GNU Stow. Manage your dotfiles, system configurations, and user settings"
        , "with type safety and atomic operations."
        , ""
        , "USAGE:"
        , "    mycfg [OPTIONS] <COMMAND>"
        , ""
        , "GLOBAL OPTIONS:"
        , "    -v, --verbose          Enable verbose output"
        , "    -q, --quiet           Suppress output except errors"
        , "    -j, --json            Output in JSON format"
        , "    -c, --config FILE     Configuration file path"
        , "    -s, --state-dir DIR   State directory path"
        , "    -l, --log-level LEVEL Set logging level (debug, info, warn, error)"
        , "    --help                 Show help message"
        , ""
        , "COMMANDS:"
        , "    init                  Initialize a new configuration"
        , "    apply                 Apply configuration changes"
        , "    rollback              Rollback to a previous generation"
        , "    validate              Validate configuration"
        , "    list                  List configurations, modules, or generations"
        , "    diff                  Show differences between configurations"
        , "    doctor                Diagnose and fix configuration issues"
        , "    help                  Show help information"
        , ""
        , "EXAMPLES:"
        , "    mycfg init                                    Initialize configuration"
        , "    mycfg apply --dry-run                         Preview changes"
        , "    mycfg rollback --generation gen-12345          Rollback to specific generation"
        , "    mycfg list generations                        List all generations"
        , "    mycfg validate                                 Validate configuration"
        , ""
        , "For more information on a specific command, use:"
        , "    mycfg <command> --help"
        ]

showCommandHelp :: Text -> Text
showCommandHelp command = case command of
    "init" -> showInitHelp
    "apply" -> showApplyHelp
    "rollback" -> showRollbackHelp
    "validate" -> showValidateHelp
    "list" -> showListHelp
    "diff" -> showDiffHelp
    "doctor" -> showDoctorHelp
    _ -> showUsage

showUsage :: Text
showUsage =
    Text.unlines
        [ "USAGE:"
        , "    mycfg [OPTIONS] <COMMAND>"
        , ""
        , "Use 'mycfg --help' for more information."
        ]

showInitHelp :: Text
showInitHelp =
    Text.unlines
        [ "NAME:"
        , "    init - Initialize a new mycfg configuration"
        , ""
        , "SYNOPSIS:"
        , "    mycfg init [OPTIONS]"
        , ""
        , "DESCRIPTION:"
        , "    Creates a new mycfg configuration directory structure with default"
        , "    configuration file and state directory. This is the first step to"
        , "    start using mycfg for managing your configuration."
        , ""
        , "OPTIONS:"
        , "    -c, --config FILE     Configuration file path (default: ~/.config/mycfg/config.toml)"
        , "    -s, --state-dir DIR   State directory path (default: ~/.local/share/mycfg)"
        , "    -e, --example         Create example configuration"
        , "    -f, --force           Force initialization even if directory exists"
        , "    --help                Show help message"
        , ""
        , "EXAMPLES:"
        , "    mycfg init                                    Initialize with defaults"
        , "    mycfg init --example                          Create with example config"
        , "    mycfg init --config ~/.config/mycfg/custom.toml  Custom config path"
        , "    mycfg init --force                            Force overwrite"
        ]

showApplyHelp :: Text
showApplyHelp =
    Text.unlines
        [ "NAME:"
        , "    apply - Apply configuration changes"
        , ""
        , "SYNOPSIS:"
        , "    mycfg apply [OPTIONS]"
        , ""
        , "DESCRIPTION:"
        , "    Applies the current configuration to the system. This command will"
        , "    compute the differences between the current state and desired state,"
        , "    create an execution plan, and apply the changes atomically."
        , ""
        , "OPTIONS:"
        , "    -n, --dry-run         Show what would be applied without making changes"
        , "    -f, --force           Force apply even if validation fails"
        , "    -b, --backup           Create backup before applying"
        , "    -v, --validate         Validate configuration before applying"
        , "    -c, --continue-on-error Continue applying even if some steps fail"
        , "    -r, --max-retries INT Maximum number of retries for failed operations (default: 3)"
        , "    --help                Show help message"
        , ""
        , "EXAMPLES:"
        , "    mycfg apply                                    Apply configuration"
        , "    mycfg apply --dry-run                         Preview changes"
        , "    mycfg apply --backup                          Create backup first"
        , "    mycfg apply --force                           Force apply"
        , "    mycfg apply --max-retries 5                  Retry failed operations 5 times"
        ]

showRollbackHelp :: Text
showRollbackHelp =
    Text.unlines
        [ "NAME:"
        , "    rollback - Rollback to a previous generation"
        , ""
        , "SYNOPSIS:"
        , "    mycfg rollback [OPTIONS]"
        , ""
        , "DESCRIPTION:"
        , "    Rolls back the system to a previous configuration generation. This"
        , "    command will restore the system to the state it was in when the"
        , "    specified generation was created."
        , ""
        , "OPTIONS:"
        , "    -g, --generation ID    Target generation ID for rollback"
        , "    -s, --snapshot ID     Target snapshot ID for rollback"
        , "    -f, --force           Force rollback even if validation fails"
        , "    -n, --dry-run         Show what would be rolled back without making changes"
        , "    -b, --backup           Create backup before rollback"
        , "    -v, --validate         Validate after rollback"
        , "    -r, --max-retries INT Maximum number of retries for failed operations (default: 3)"
        , "    --help                Show help message"
        , ""
        , "EXAMPLES:"
        , "    mycfg rollback --generation gen-12345          Rollback to specific generation"
        , "    mycfg rollback --snapshot snap-67890           Rollback to snapshot"
        , "    mycfg rollback --dry-run                      Preview rollback"
        , "    mycfg rollback --backup                        Create backup first"
        , "    mycfg rollback                                Rollback to previous generation"
        ]

showValidateHelp :: Text
showValidateHelp =
    Text.unlines
        [ "NAME:"
        , "    validate - Validate configuration"
        , ""
        , "SYNOPSIS:"
        , "    mycfg validate [OPTIONS]"
        , ""
        , "DESCRIPTION:"
        , "    Validates the current configuration for syntax errors, schema"
        , "    violations, and logical inconsistencies. This command is useful"
        , "    for catching configuration issues before applying changes."
        , ""
        , "OPTIONS:"
        , "    -c, --config FILE     Configuration file to validate"
        , "    -s, --strict          Enable strict validation mode"
        , "    -w, --warnings         Show validation warnings"
        , "    --help                Show help message"
        , ""
        , "EXAMPLES:"
        , "    mycfg validate                                 Validate default config"
        , "    mycfg validate --config custom.toml            Validate specific file"
        , "    mycfg validate --strict                       Strict validation"
        , "    mycfg validate --warnings                      Show warnings"
        ]

showListHelp :: Text
showListHelp =
    Text.unlines
        [ "NAME:"
        , "    list - List configurations, modules, or generations"
        , ""
        , "SYNOPSIS:"
        , "    mycfg list <TYPE> [OPTIONS]"
        , ""
        , "DESCRIPTION:"
        , "    Lists various types of items managed by mycfg. This command"
        , "    can show configuration generations, available modules, profiles,"
        , "    or snapshots."
        , ""
        , "TYPES:"
        , "    generations           List configuration generations"
        , "    modules              List available modules"
        , "    profiles             List configuration profiles"
        , "    snapshots            List snapshots"
        , ""
        , "OPTIONS:"
        , "    -d, --details          Show detailed information"
        , "    --format FORMAT       Output format (table, json, yaml)"
        , "    --help                Show help message"
        , ""
        , "EXAMPLES:"
        , "    mycfg list generations                        List all generations"
        , "    mycfg list modules --details                List modules with details"
        , "    mycfg list profiles --format json           List profiles in JSON"
        , "    mycfg list snapshots                         List snapshots"
        ]

showDiffHelp :: Text
showDiffHelp =
    Text.unlines
        [ "NAME:"
        , "    diff - Show differences between configurations"
        , ""
        , "SYNOPSIS:"
        , "    mycfg diff [OPTIONS]"
        , ""
        , "DESCRIPTION:"
        , "    Shows the differences between two configuration generations or"
        , "    between the current configuration and a previous generation."
        , "    This is useful for understanding what changes will be applied."
        , ""
        , "OPTIONS:"
        , "    -f, --from ID         From generation ID"
        , "    -t, --to ID           To generation ID"
        , "    -c, --changes-only     Show only changed files"
        , "    --format FORMAT       Output format (table, json, yaml)"
        , "    --help                Show help message"
        , ""
        , "EXAMPLES:"
        , "    mycfg diff                                     Show current vs previous"
        , "    mycfg diff --from gen-12345 --to gen-67890  Compare two generations"
        , "    mycfg diff --changes-only                      Show only changed files"
        , "    mycfg diff --format json                       Output in JSON format"
        ]

showDoctorHelp :: Text
showDoctorHelp =
    Text.unlines
        [ "NAME:"
        , "    doctor - Diagnose and fix configuration issues"
        , ""
        , "SYNOPSIS:"
        , "    mycfg doctor [OPTIONS]"
        , ""
        , "DESCRIPTION:"
        , "    Diagnoses common configuration issues and attempts to fix them."
        , "    This command checks configuration validity, state consistency,"
        , "    module dependencies, and other potential problems."
        , ""
        , "OPTIONS:"
        , "    -a, --all             Check all aspects of configuration"
        , "    -c, --config          Check configuration validity"
        , "    -s, --state           Check state consistency"
        , "    -m, --modules         Check module dependencies"
        , "    -f, --fix              Attempt to fix detected issues"
        , "    --help                Show help message"
        , ""
        , "EXAMPLES:"
        , "    mycfg doctor                                   Run all checks"
        , "    mycfg doctor --config                          Check configuration only"
        , "    mycfg doctor --state                           Check state only"
        , "    mycfg doctor --modules                         Check modules only"
        , "    mycfg doctor --fix                              Attempt to fix issues"
        ]

showConfigurationHelp :: Text
showConfigurationHelp =
    Text.unlines
        [ "CONFIGURATION"
        , ""
        , "mycfg uses TOML configuration files to define the desired system state."
        , "The configuration file is typically located at ~/.config/mycfg/config.toml"
        , ""
            "BASIC STRUCTURE:"
        , "[system]"
        , "hostname = \"my-hostname\""
        , "timezone = \"UTC\""
        , "locale = \"en_US\""
        , "shell = \"bash\""
        , "editor = \"vim\""
        , ""
        , "[files]"
        , "\".config/nvim\" = \"./dotfiles/nvim\""
        , "\".zshrc\" = \"./dotfiles/zshrc\""
        , "\".gitconfig\" = \"./dotfiles/gitconfig\""
        , ""
        , "[packages]"
        , "cli = [\"git\", \"ripgrep\", \"fd\", \"exa\"]"
        , "gui = [\"firefox\", \"vscode\"]"
        , "development = [\"ghc\", \"cabal\", \"stack\"]"
        , "system = []"
        , ""
        , "[services]"
        , "git.enable = true"
        , "ssh.enable = true"
        , ""
        , "modules = [\"git\", \"neovim\", \"zsh\"]"
        , ""
        , "[profiles.default]"
        , "name = \"default\""
        , "description = \"Default configuration profile\""
        , "modules = [\"git\", \"neovim\", \"zsh\"]"
        , ""
            "For more detailed configuration examples, see the documentation."
        ]

showModulesHelp :: Text
showModulesHelp =
    Text.unlines
        [ "MODULES"
        , ""
        , "Modules are reusable configuration components that can be included in"
        , "your main configuration. They provide a way to organize and share"
        , "configuration logic."
        , ""
            "MODULE STRUCTURE:"
        , "Each module is a directory containing:"
        , "  - module.toml    Module metadata and configuration"
        , "  - files/         Files to be managed by the module"
        , "  - scripts/       Optional scripts for setup/teardown"
        , ""
            "MODULE METADATA (module.toml):"
        , "[module]"
        , "name = \"git\""
        , "version = \"1.0.0\""
        , "description = \"Git configuration module\""
        , "author = \"Your Name\""
        , "license = \"MIT\""
        , "dependencies = []"
        , "provides = [\"git-config\"]"
        , "conflicts = []"
        , ""
            "USING MODULES:"
        , "Add modules to your configuration:"
        , "modules = [\"git\", \"neovim\", \"zsh\"]"
        , ""
            "MODULE DISCOVERY:"
        , "Modules are searched in:"
        , "  - ~/.config/mycfg/modules/"
        , "  - /usr/share/mycfg/modules/"
        , "  - Additional paths specified with --module-path"
        , ""
            "For more information on creating modules, see the documentation."
        ]

showTroubleshootingHelp :: Text
showTroubleshootingHelp =
    Text.unlines
        [ "TROUBLESHOOTING"
        , ""
        , "COMMON ISSUES:"
        , ""
        , "Permission Denied:"
        , "  Ensure you have write permissions to target directories"
        , "  Use sudo only when necessary for system-wide changes"
        , ""
        , "Configuration Parse Errors:"
        , "  Check TOML syntax with online validator"
        , "  Ensure all required fields are present"
        , "  Verify file paths are correct and accessible"
        , ""
        , "Module Not Found:"
        , "  Check module search paths"
        , "  Verify module name spelling"
        , "  Ensure module.toml exists in module directory"
        , ""
        , "State Corruption:"
        , "  Run 'mycfg doctor --all' to diagnose issues"
        , "  Backup current state before attempting fixes"
        , "  Consider reinitializing if corruption is severe"
        , ""
        , "Rollback Failures:"
        , "  Check if target generation exists"
        , "  Verify sufficient disk space for rollback"
        , "  Ensure no processes are using affected files"
        , ""
        , "GETTING HELP:"
        , "  Use 'mycfg doctor' for automated diagnostics"
        , "  Check logs in ~/.local/share/mycfg/logs/"
        , "  Enable verbose output with --verbose flag"
        , "  Use --dry-run to preview operations"
        , ""
        , "DEBUG MODE:"
        , "  Set log level to debug: --log-level debug"
        , "  Enable JSON output for structured logging: --json"
        , "  Check state directory contents manually"
        , ""
        , "If issues persist, consider:"
        , "  - Creating a backup of your current state"
        , "  - Reinitializing the configuration"
        , "  - Seeking help from the community"
        ]

showExamples :: Text
showExamples =
    Text.unlines
        [ "EXAMPLES"
        , ""
        , "BASIC WORKFLOW:"
        , ""
        , "1. Initialize configuration:"
        , "   mycfg init --example"
        , ""
        , "2. Edit configuration:"
        , "   ~/.config/mycfg/config.toml"
        , ""
        , "3. Validate configuration:"
        , "   mycfg validate"
        , ""
        , "4. Preview changes:"
        , "   mycfg apply --dry-run"
        , ""
        , "5. Apply configuration:"
        , "   mycfg apply"
        , ""
        , "6. List generations:"
        , "   mycfg list generations"
        , ""
        , "7. Rollback if needed:"
        , "   mycfg rollback --generation gen-12345"
        , ""
        , "ADVANCED EXAMPLES:"
        , ""
        , "Force apply with backup:"
        , "   mycfg apply --force --backup"
        , ""
        , "Apply with custom config:"
        , "   mycfg apply --config ./custom.toml"
        , ""
        , "Compare generations:"
        , "   mycfg diff --from gen-12345 --to gen-67890"
        , ""
        , "List available modules:"
        , "   mycfg list modules --details"
        , ""
        , "Run comprehensive check:"
        , "   mycfg doctor --all --fix"
        , ""
        , "JSON output for automation:"
        , "   mycfg list generations --format json"
        , ""
        , "Verbose logging for debugging:"
        , "   mycfg apply --verbose --log-level debug"
        ]
