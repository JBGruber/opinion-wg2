import os
import time

import pyautogui

PDF = "10.1007_978-3-030-01057-7_62.pdf"

print("Starting scraping pipeline...")
os.system("firefox")


# open new tab and navigate to chatgpt
pyautogui.hotkey("ctrl", "t")
time.sleep(1)
pyautogui.write("chatgpt.com".lower(), interval=0)
pyautogui.press("enter")

# click on attach file and then "upload from computer"
time.sleep(3)
# pyautogui.click("attach.png")
pyautogui.click(x=750, y=1045)
time.sleep(3)
# pyautogui.press("down", presses=2, interval=0.25)
pyautogui.moveTo(x=770, y=1000)
pyautogui.click(x=770, y=990)

# type file name into selection
pyautogui.write(PDF, interval=0)
time.sleep(1)
pyautogui.press("enter")

# after upload, ask question
time.sleep(10)
pyautogui.write(
    "IIs an Opinion Measurement Tool (OMT) applied or developed in this paper? Answer only with <yes> or <no>",
    interval=0.25,
)
pyautogui.press("enter")

# wait for text generation, then save page
time.sleep(30)
pyautogui.hotkey("ctrl", "s")
time.sleep(1)
pyautogui.write(PDF, interval=0)
pyautogui.press("enter")
