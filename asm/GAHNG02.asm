*          DATA SET GAHNG02    AT LEVEL 002 AS OF 08/22/00                      
*PHASE TB0502A                                                                  
         TITLE 'HANGMAN GAME - DICTIONARY PART 2'                               
HANGMAN2 CSECT                                                                  
         DC    C'FABULOUS FACILITATE FACTOTUM FAILURE FAINTNES'                 
         DC    C'S FAITH FALCON FALLIBLE FALSIFY FAMILY FANATI'                 
         DC    C'C FANTASTIC FARTHER FASCIST FASTEN FATHOM FAU'                 
         DC    C'LTLESS FAVOURABLE FEARLESS FEDERAL FEELING FE'                 
         DC    C'LLOWSHIP FEMININE FESTIVAL FEVER FIDELITY FIE'                 
         DC    C'RCE FIGURE FILLET FINANCIER FINDING FINESSE F'                 
         DC    C'INISH FIREMAN FIRM FIRMAMENT FISHERMAN FISSUR'                 
         DC    C'E FIXED FLAGRANT FLAMBOYANT FLARE FLASK FLATT'                 
         DC    C'ER FLAVOUR FLICKER FLIPPANT FLIRT FLOATING FL'                 
         DC    C'OURISH FLOWER FLUORESCENT FLUTTER FOAM FOETUS'                 
         DC    C' FOLIAGE FOLLOWER FOOLISH FOOTFALL FORAGE FOR'                 
         DC    C'BEAR FORECLOSE FOREIGNER FOREMOST FORFEIT FOR'                 
         DC    C'MAL FORMULA FORWARD FOSTER FOUNDATION FOUNTAI'                 
         DC    C'N FRACTION FRANCISE FREEDOM FREEZING FRESHEN '                 
         DC    C'FRIENDSHIP FROLIC FRONTIER FRUGAL FRUSTRATE F'                 
         DC    C'ULCRUM FUNCTION FUNNEL FURNISH FUTURE GADGET '                 
         DC    C'GALAXY GALLERY GAMBIT GARBAGE GARNISH GASTRON'                 
         DC    C'OME GAUGE GENERAL GENERATION GENTLE GENUINE G'                 
         DC    C'ERMINATE GESTURE GIANT GIGANTIC GINGERLY GIRD'                 
         DC    C'LE GLACIER GLANCE GLIMMER GLORIOUS GLOSSARY G'                 
         DC    C'NOME GODDESS GOING GOLDEN GONDOLA GORGEOUS GO'                 
         DC    C'URMET GRACIOUS GRADUATE GRAMMAR GRANDFATHER G'                 
         DC    C'RAPHITE GRATITUDE GRAVITY GREATNESS GREGARIOU'                 
         DC    C'S GRIMACE GROCER GROTESQUE GROUNDWORK GROWER '                 
         DC    C'GUARANTEE GUESS GUILTY GUNPOWDER GYMNASIUM HA'                 
         DC    C'BITUAL HAGGLE HALCYON HALLUCINATION HAMMER HA'                 
         DC    C'NDICAP HANDSOME HANGER HAPPENING HARASS HARBO'                 
         DC    C'UR HAREM HARLOT HARVEST HASTEN HATCHET HAUNTE'                 
         DC    C'D HAZARD HEAD HEADACHE HEARING HEART HEAVENLY'                 
         DC    C' HEDONIST HELICOPTER HEMISPHERE HERCULEAN HER'                 
         DC    C'EDITARY HEROIC HIBERNATE HIDEOUS HIGHEST HIND'                 
         DC    C'ER HISTORIAN HOCKEY HOLIDAY HOLOCAUST HOMAGE '                 
         DC    C'HOMELESS HOMESTEAD HONEYMOON HOOLIGAN HORIZON'                 
         DC    C' HORROR HORSEMANSHIP HOSTILITY HOUND HOUSEHOL'                 
         DC    C'D HOUSEWIFE HUDDLE HUMANE HUMILITY HUNDRED HU'                 
         DC    C'SBAND HYBRID HYSTERIA ICICLE IDEOLOGY IGNORE '                 
         DC    C'ILLEGITIMATE ILLUMINATE ILLUSTRATE IMITATION '                 
         DC    C'IMMODEST IMPEACH IMPERFECT IMPERSONATE IMPORT'                 
         DC    C'ANCE IMPOTENCE IMPRESSION IMPROPER IMPULSE IN'                 
         DC    C'ABILITY INANE INAUGURAL INCIDENT INCLINE INCO'                 
         DC    C'ME INCONGRUOUS INCONSISTENT INCREDIBLE INCUR '                 
         DC    C'INDELIBLE INDEX INDICT INDIVIDUAL INDULGE INE'                 
         DC    C'RTIA INEXPENSIVE INFECT INFINITE INFLUENCE IN'                 
         DC    C'FORMATION INGRAIN INHABIT INJECT INNER INQUES'                 
         DC    C'T INSECT INSIGNIA INSPECT INSTEAD INSTRUMENT '                 
         DC    C'INSULT INTELLECTUAL INTERCEPT INTEREST INTERM'                 
         DC    C'EDIATE INTERPOSE INTIMATE INTONATION INTRODUC'                 
         DC    C'TION INTRUDER INVALID INVENTORY INVOICE IRATE'                 
         DC    C' IRREGULAR ISOBAR ISSUE IVORY JACKET JAUNDICE'                 
         DC    C'D JEALOUSY JETTISON JOINT JOLLY JUBILANT JUGG'                 
         DC    C'ERNAUT JUNCTION JUSTICE JUVENILE KANGAROO KEN'                 
         DC    C'NEL KEROSENE KIDNEY KINDLE KINGDOM KNEAD KNIG'                 
         DC    C'HT KNOWLEDGE KNUCKLE LABORATORY LACERATE LADD'                 
         DC    C'ER LAGGARD LAGOON LAMINATE LANGUAGE LANTERN L'                 
         DC    C'ARCENY LASCIVIOUS LATELY LATITUDE LAUGHTER LA'                 
         DC    C'VENDAR LAWYER LAYMAN LEAF LEAGUE LEARNING LEA'                 
         DC    C'THER LECTURE LEGACY LEGISLATE LEISURE LEOPARD'                 
         DC    C' LESSON LETHAL LETTER LIBERAL LIBRETTO LIEUTE'                 
         DC    C'NANT LIFELONG LIGHTNING LIMIT LINEAR LINGUIST'                 
         DC    C'IC LINKED LIQUIDATE LITERAL LITTER LIVELY LOC'                 
         DC    C'ATION LODGER LOGICAL LONGITUDE LOOKING LOOSE '                 
         DC    C'LOSING LOTION LOVABLE LOYAL LUCID LUMINOUS LU'                 
         DC    C'NATIC LUXURY MACHINE MAGICIAN MAGISTRATE MAID'                 
         DC    C'EN MAINTAIN MAJORITY MAKE MALADY MALEVOLENT M'                 
         DC    C'ALINGER MANAGE MANIFESTO MANOEUVRE MANUSCRIPT'                 
         DC    C' MARGIN MARINER MARKET MARRIAGE MARTYR MASSAC'                 
         DC    C'RE MASSIVE MASTER MATERIAL MATRIARCH MATURITY'                 
         DC    C' MAYONNAISE MEANING MEASURE MEDICINE MEDITATE'                 
         DC    C' MEGAPHONE MEMBER MEMORY MERCENARY MERCIFUL M'                 
         DC    C'ESSAGE METALLIC METROPOLIS MICROSCOPE MIGHTY '                 
         DC    C'MILITANT MILLION MIMEOGRATH MINERAL MINIMUM M'                 
         DC    C'INISTRY MINUTE MIRACLE MISERY MISNOMER MISSIL'                 
         DC    C'E MISTRESS MNEMONIC MODERATE MODICUM MOLECULA'                 
         DC    C'R MOMENTUM MONOGRAM MONOTONOUS MONUMENT MORAL'                 
         DC    C' MORATORIUM MORNING MOSAIC MOTHER MOUNTAIN MO'                 
         DC    C'USETRAP MOUTHFUL MOVEMENT MUDDLE MUFFLER MULT'                 
         DC    C'IPLY MULTINATIONAL MUSCULAR MUSHROOM MUTILATE'                 
         DC    C' MYSTERIOUS MYSTERY NAMELESS NARCOTIC NATIVE '                 
         DC    C'NATURAL NAVIGATE NEARNESS NECESSITY NECTAR NE'                 
         DC    C'IGHBOUR NEUROTIC NEVER NIBBLE NIHILIST BKEND '                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002GAHNG02   08/22/00'                                      
         END                                                                    
