library(miniUI)

#miniPage is fundamentally different than the traditional fluidPage. 
#miniPage tries to exactly fill its containing browser window or frame
#miniPage does nothing but display its children
#miniPage is a special kind of container called a "flex box"
#UI objects put inside miniPage: gadgetTitleBar/miniTitleBar, miniTabstripPanel, miniButtonBlock, miniContentPanel

#gadgetTitleBar: it adds a header that displays the name of gadget, "Cancel" and "Done" buttons

#miniContentPanel: "bridge" between the strange flex box layout that miniPage creates, and normal HTML elements and controls that aren't flex box aware
                   "You can put a plot inside of miniContentPanel"
                   
#miniTabstripPanel is extremely useful for giving your app multiple screens
