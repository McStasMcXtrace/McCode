from django.core.management.base import NoArgsCommand, make_option

import glob, re


def grab_list():
    return glob.glob('sim/*.instr')


def gen_html(instr):
    src = file(instr).read()
    doc = re.search(r'^/\*\*+(.*)\%[Ee]', src,
                    re.MULTILINE + re.DOTALL).group(1)

    out = instr + '.doc.txt'
    file(out, 'w').write(doc)


def main():
    map(gen_html, grab_list())


class Command(NoArgsCommand):

    help = "Whatever you want to print here"

    option_list = NoArgsCommand.option_list + (
        make_option('--verbose', action='store_true'),
    )

    def handle_noargs(self, **options):
        main()
