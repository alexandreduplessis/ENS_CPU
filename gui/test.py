# importing required librarie
import sys
import subprocess
from PySide6.QtWidgets import QApplication, QWidget
from PySide6.QtWidgets import QVBoxLayout, QLabel
from PySide6.QtGui import QFont
from PySide6.QtCore import QTimer, QTime, QObject, Qt, Signal, Slot, QThread, QProcess

class Window(QWidget):
  
    def __init__(self):
        super().__init__()
  
        # setting geometry of main window
        self.setGeometry(100, 100, 800, 400)
  
        # creating a vertical layout
        layout = QVBoxLayout()
  
        # creating font object
        font = QFont('Arial', 120, QFont.Bold)
  
        # creating a label object
        self.label = QLabel()
  
        # setting centre alignment to the label
        self.label.setAlignment(Qt.AlignCenter)
  
        # setting font to the label
        self.label.setFont(font)
  
        # adding label to the layout
        layout.addWidget(self.label)
  
        # setting the layout to main window
        self.setLayout(layout)
  	
        self.process = QProcess()
        self.process.readyRead.connect(self.onReadyReadStandardOutput)

    def run(self):
       self.process.start("./a.out")

    def onReadyReadStandardOutput(self):
        result = self.process.readAll().data().decode()
        self.label.setText(result)

# create pyqt5 app
App = QApplication(sys.argv)
  
# create the instance of our Window
window = Window()
  
# showing all the widgets
window.show()
window.run()  
# start the app
App.exit(App.exec_())
