# importing required librarie
import sys
import subprocess
import re
from PySide6.QtWidgets import QApplication, QWidget
from PySide6.QtWidgets import QVBoxLayout, QLabel
from PySide6.QtGui import QFont
from PySide6.QtCore import QTimer, QTime, QObject, Qt, Signal, Slot, QThread, QProcess

def complement_to_1000(s):
    if int(s) < 10:
        return "000"+s
    elif int(s) < 100:
        return "00"+s
    elif int(s) < 1000:
        return "0"+s
    else:
        return s

def complement_to_10(s):
    if int(s) < 10:
        return "0"+s
    else:
        return s

def str_to_signed_int(s):
    if int(s[0]):
        return -1*(2**15-int(s[1:], 2))
    else:
        return int(s[1:], 2)

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
        pattern_1 = r'register_0 = ([01]{16})'
        pattern_2 = r'register_1 = ([01]{16})'
        pattern_3 = r'register_2 = ([01]{16})'
        pattern_4 = r'register_3 = ([01]{16})'
        pattern_5 = r'register_4 = ([01]{16})'
        pattern_6 = r'register_5 = ([01]{16})'
        r1 = str_to_signed_int(re.search(pattern_1, result).group(1))
        r2 = str_to_signed_int(re.search(pattern_2, result).group(1))
        r3 = str_to_signed_int(re.search(pattern_3, result).group(1))
        r4 = str_to_signed_int(re.search(pattern_4, result).group(1))
        r5 = str_to_signed_int(re.search(pattern_5, result).group(1))
        r6 = str_to_signed_int(re.search(pattern_6, result).group(1))
        self.label.setText(complement_to_1000(str(r6))
                           +"-"+complement_to_10(str(r5))
                           +"-"+complement_to_10(str(r4))
                           +" "+complement_to_10(str(r3))
                           +":"+complement_to_10(str(r2))
                           +":"+complement_to_10(str(r1)))

# create pyqt5 app
App = QApplication(sys.argv)
  
# create the instance of our Window
window = Window()
  
# showing all the widgets
window.show()
window.run()  
# start the app
App.exit(App.exec_())
