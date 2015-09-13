#!/usr/bin/python
import re, os

# scheme 1
def get_password_emacs(machine, login, port):
    s = "machine %s login %s port %s password ([^ ]*)\n" % (machine, login, port)
    p = re.compile(s)

    # On Mac OS X, you can use the system keychain to store the passphrase; so the python line becomes:
    # authinfo = os.popen("gpg -q -d --no-mdc-warning --no-tty --passphrase `security find-generic-password -a authinfo -s offlineimap -w` ~/.authinfo.gpg").read()

    #authinfo = os.popen("gpg -q --no-tty -d ~/.authinfo.gpg").read()
    authinfo = os.popen("gpg -q -d ~/.authinfo.gpg").read()
    return p.search(authinfo).group(1)

##resp = get_password_emacs("imap.gmail.com", "robertzhouxh@gmail.com", "993")
##print(resp)


# scheme 2
# gpg --default-recipient-self -e /path/to/plain/password
# mv /path/to/plain/password ~/.offlineimappass.gpg
def get_pass():
    return check_output("gpg -dq ~/.offlineimappass.gpg", shell=True).strip("\n")
