package net.fortytwo.conlang;

public class Phonemes {

    public static final PhonemeCollection SINGLE_INITIAL_CONSONANTS = new PhonemeCollection(
            "b",
            "p",
            "d",
            "t",
            "g",
            "k",
            "v",
            "f",
            "z",
            "s",
            "j",
            "c",
            "h",
            "m",
            "n",
            "ng",
            "l",
            "r",
            "w",
            "y");

    public static final PhonemeCollection PAIRED_INITIAL_CONSONANTS = new PhonemeCollection(
            "bl", "br", "bw?", "by", "(bv)",
            "pl", "pr", "pw?", "py?", "(pc)", "(pf)", "(ps)",
            "dl?", "dr?", "dw?", "dy?", "(dj)", "(dv)", "(dz)",
            "tl?", "tr", "tw?", "ty", "(tc)", "(tf)", "(ts)", "(tv)",
            "gl", "gr", "gw?", "gy?",
            "kl", "kr", "kw?", "ky?", "(kc)", "(kf)", "(ks)",
            "vl?", "vr", "vw?", "vy?",
            "fl", "fr?", "fw?", "fy", "(fk)", "fm?", "fp?", "(ft)",
            "(zl)", "(zr)", "zw?", "zy?", "(zn)", "zv?",
            "(sl)", "(sr)", "sw?", "(sy)", "sf?", "sk", "(sn)", "sp?", "st?",
            "(jl)", "(jr)", "jw?", "jy?",
            "(cl)", "cr?", "cw?", "cy?", "cf?", "ck?", "(cm)", "(cn)", "cp?", "ct?",
            "hl?", "hr?", "hw?", "hy", "hf?", "(hm)", "(hn)", "(ht)");

    public static final PhonemeCollection SINGLE_FINAL_CONSONANTS = new PhonemeCollection(
            "b?",
            "d",
            "g",
            "p?",
            "t",
            "k",
            "v",
            "z",
            "j?",
            "f?",
            "s?",
            "c?",
            "m",
            "n",
            "ng?",
            "l",
            "r");

    public static final PhonemeCollection PAIRED_FINAL_CONSONANTS = new PhonemeCollection(
            "(fp)", "ft", "(fk)", "(fs)", "(fc)", "(fm)", "(fn)", "(fl)", "(fr)", "(fw)", "(fy)",
            "sp", "st", "sk", "(sf)", "(sm)", "(sn)", "(sl)", "(sr)", "(sw)", "(sy)",
            "cp?", "ct?", "ck?", "(cf)", "(cm)", "(cn)", "(cl)", "(cr)", "(cw)", "(cy)",
            "mb?", "md?", "mg?", "mp", "mt?", "mk?", "mv?", "mz?", "mj?", "mf?", "ms?", "mc?", "(mn)", "(mng)", "(ml)", "(mr)", "(mw)", "(my)",
            "(nb)", "nd", "(np)", "nt", "nv?", "nz?", "nj?", "nf?", "ns?", "nc?", "(nm)", "(nl)", "(nr)", "(nw)", "(ny)",
            "ng?", "nk", "(ngm)", "(ngn)", "(nng)", "(ngl)", "(ngr)", "(ngw)", "(ngy)",
            "lb?", "ld", "lg", "lp", "lt", "lk", "lv", "lz?", "lj?", "lf?", "ls?", "lc?", "lm", "ln", "(lr)", "(lw)", "(ly)",
            "rb?", "rd", "rg", "rp?", "rt", "rk", "rv", "rz?", "rj?", "rf?", "rs?", "rc?", "rm", "rn", "(rng)", "(rl)", "(rw)", "(ry)");

    public static final PhonemeCollection LONG_VOWELS = new PhonemeCollection(
            "a", "e", "i", "o", "u", "oe", "ue");

    public static final PhonemeCollection SHORT_VOWELS = new PhonemeCollection(
            "a", "e", "i", "o", "u", "oe", "ue");

    public static final PhonemeCollection DIPHTHONGS = new PhonemeCollection(
            "au", "ai",
            "eu", "ei?",
            "iu",
            "ou?", "oi",
            "ui?",
            "oei",
            "uei?");

    public static final PhonemeCollection VERB_SUFFIXES = new PhonemeCollection(
            "a", "e", "i", "o"
    );

    // TODO: incomplete
    public static final PhonemeCollection NOUN_SUFFIXES = new PhonemeCollection(
            "au", "ai",
            "ya", "yo?", "yoe?", "wa?",
            "en", "em", "er",
            "el", "ien", "ing",
            "len", "lem", "ler",
            "ni", "mi", "ri"
    );
}
