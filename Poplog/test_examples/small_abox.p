vars small_premises = [
[os1 NEAR os2]
[sm2 FAR sm1]
[os3 NEAR os4]
[os3 FAR os5]
[os1 BPT os1]
[os2 BPT os2]
[os3 BPT os3]
[os4 BPT os4]
[os5 BPT os5]
[sm1 BPT sm1]
[sm2 BPT sm2]
[sm3 BPT sm3]
];

vars small_abox = [
[os1 BPT sm1]
[os2 BPT sm2]
[os1 BEQ sm1]
[os4 BPT sm3]
[sm3 BPT os5]
];

