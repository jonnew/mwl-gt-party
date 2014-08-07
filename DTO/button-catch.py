# Get the identity of a button press

evt_file = open("/dev/input/event1", "rb")
while True:
    evt = evt_file.read(16) # Read the event
    evt_file.read(16)       # Discard the debounce event 
    code = ord(evt[10])
    if ord(evt[12]): 
    	print "Switch "+str(code)
