# Change Konsole color scheme
## Change Konsole default profile color scheme
PROFILE_NAME="Chabam-light"
kwriteconfig5 --file konsolerc --group "Desktop Entry" --key DefaultProfile "$PROFILE_NAME.profile"

## Apply the new Konsole profile to all open Konsole windows
echo "Setting Konsole profile to $PROFILE_NAME"
### Get list of all Konsole services
KONSOLE_SERVICES=$(qdbus | grep org.kde.konsole)

### Apply the new profile to all windows and their sessions/tabs
for service in $KONSOLE_SERVICES; do
    # Get list of all windows under each service
    echo "Service: $service"
    WINDOWS=$(qdbus $service / | grep -oP '/Windows/\d+')
    echo "Windows: $WINDOWS"
    SESSIONS=$(qdbus $service | grep -oP '/Sessions/\d+')
    echo $SESSIONS
    for session in $SESSIONS; do
        qdbus $service $session setProfile "$PROFILE_NAME"
    done
done
