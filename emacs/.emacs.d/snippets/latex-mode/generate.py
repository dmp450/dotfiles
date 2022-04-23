import string


def blackboard_letters():
    for c in string.ascii_uppercase:
        template = "# -*- mode: snippet -*-\n# contributor: Derek Perrin <derek@derekperrin.com>\n"
        name = f'bold_{c}'
        key = c + c
        template += f'# name: {name}\n# key: {key}\n# --\n\\mathbb{{{c}}}'
        with open(name, 'w') as f:
            f.write(template)

def fraktur_letters():
    for c in string.ascii_letters:
        template = "# -*- mode: snippet -*-\n# contributor: Derek Perrin <derek@derekperrin.com>\n"
        name = f'frak_{c}'
        key = "fr" + c
        template += f'# name: {name}\n# key: {key}\n# --\n\\mathfrak{{{c}}}'
        with open(name, 'w') as f:
            f.write(template)

def calligraphic_letters():
    for c in string.ascii_uppercase:
        template = "# -*- mode: snippet -*-\n# contributor: Derek Perrin <derek@derekperrin.com>\n"
        name = f'cal_{c}'
        key = "c" + c
        template += f'# name: {name}\n# key: {key}\n# --\n\\mathcal{{{c}}}'
        with open(name, 'w') as f:
            f.write(template)
    
if __name__ == "__main__":
    blackboard_letters()
    fraktur_letters()
    calligraphic_letters()
