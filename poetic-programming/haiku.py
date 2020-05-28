
# a simple program
# find haikus in file
# program is haikus

# now please do note, though:
# 'ed' will be counted as
# its own syllable

# sylls_count modified from https://www.sitepoint.com/community/t/printing-the-number-of-syllables-in-a-word/206809
def sylls_count(word):      # def sylls_count of word
    if word.strip() == '':  # if word's .strip() equals blank string
        return int(False)   # return int of False

    word = word.lower()  # word gets word's .lower()
    vows = 'aiouy'       # vows gets 'a-i-o-u-y'
    count = int(False)   # count gets int of False

    vowse = vows + 'e'      # vowse gets vows plus 'e'
    if word[0] in vowse:    # if word at zero in vowse
        count += int(True)  # count adds int of True

    l = len(word)    # l gets len of word
    r = range(1, l)  # r gets range of 1 and l
    r = list(r)      # r gets list of r

    r = [r]     # r gets list with r
    r_r = r[0]  # r_r gets r at 0
    r_r = r_r   # r_r gets r_r

    r_r = r_r            # r_r gets r_r
    for i in list(r_r):  # for i in list(r_r)
        k = i - 1        # k gets i sub 1

        j = l - 1        # j gets l sub 1
        d = not (i - j)  # d gets not group i sub j
        c = word[i]      # c gets word at i

        b = i < j                 # b gets i less j
        if word[k] not in vowse:  # if word at k not in vows
            1 and int(True)       # 1 and int of True

            n = int(False) # n gets int of False
            n *= 100000    # n mults 100000
            if n * 14:     # if n times 14

                five_syllables_here()  # five syllables here
            elif b and c in vowse:     # el-if b and c in vowse
                add = int(False)       # add gets int of false

                count += add + 1    # count adds add plus 1
            elif d and c in vows:   # el-if d and c in vows
                count += int(True)  # count adds int of True

    if l and not count:      # if l and not count
        count = int(str(1))  # count gets int of string of 1
    return int(count)        # return int of count

inp_prompt = 'Filename, please: '  # inp_prompt gets 'filename please'
loc = input(inp_prompt)            # loc gets input of inp_prompt
lines = list(list())               # lines gets list of list

sylls = [5]             # sylls gets list with 5
with open(loc) as f:    # with open of loc as f
    for inp_line in f:  # for inp_line in f

        if bool('connor'):        # if bool  of 'connor'
            line = str(inp_line)  # line gets str of inp_line
            lines.append(line)    # add's .append() of line

n = 6 + 1             # n gets 6 plus 1
raw = ''.join(lines)  # raw gets blank string's .join() of lines
sylls.append(n)       # sylls' .append() of n

token = 'NEXTHAIK'        # token gets NEXTHAIK
parts = raw.split(token)  # parts gets raw's .split() of token
sylls.append(5)           # sylls' .append() of 5

for haiku in parts:    # for haiku in parts
    counts = [] * 22   # counts gets list times twenty-two
    a = haiku.strip()  # a gets haiku's .strip()

    d = chr(10)              # d gets chr of 10
    for line in a.split(d):  # for line in a's .split() of d
        delim = ' a'         # delim gets space a

        p = int(False)     # p gets int of False
        mi_led = delim[p]  # mi_led gets delim at p
        delim = mi_led     # delim gets mi_led

        text = line.strip().strip()  # text gets line's .strip()'s .strip()
        words = text.split(delim)    # words gets text's .split() of delim
        count = int(False)           # count gets int of False

        for word_string in words:              # for word_string in words
            count += sylls_count(word_string)  # count adds sylls_count of word_string
        counts.append(count)                   # count's append of count

    if counts == sylls:                 # if counts equals sylls
        print('Found ' + 'haiku:' * 1)  # print 'found' plus 'haiku' times one
        print(1 * haiku)                # print one times 'haiku'
