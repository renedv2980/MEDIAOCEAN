*          DATA SET GAHNG01    AT LEVEL 002 AS OF 08/22/00                      
*PHASE TB0501A                                                                  
         TITLE 'HANGMAN GAME - DICTIONARY PART 1'                               
HANGMAN1 CSECT                                                                  
         DC    C'ABACUS ABANDON ABBEY ABDICATE ABHOR ABILITY ABLUTI'            
         DC    C'ON ABNORMAL ABORTION ABOVE ABSENTEE ABSOLUTE ABSTA'            
         DC    C'IN ABSTRACT ACCENT ACCESS ACCIDENT ACCOUNT ACCUSE '            
         DC    C'ACE ACHIEVE ACID ACQUIRE ACT ACTION ACUTE ADAGE AD'            
         DC    C'DRESS ADJUST ADMIRATION ADOPT ADULT ADVANCE ADVENT'            
         DC    C' ADVISE ADVOCATE AESTHETIC AFFAIR AFFECT AFFORD AG'            
         DC    C'AIN AGE AGENCY AGENT AGGREVATE AGONY AGREE AHEAD A'            
         DC    C'IR AJAR ALGEBRA ALIGHT ALIVE ALLEGRO ALLEVIATE ALL'            
         DC    C'IANCE ALLOY ALLURE ALMANAC ALTITUDE AMATEUR AMBER '            
         DC    C'AMIABLE AMNESTY AMPLE AMUSE ANAGRAM ANALYSIS ANECD'            
         DC    C'OTE ANGUISH ANKLE ANNUAL ANTARTIC ANTIC ANTIQUE AN'            
         DC    C'VIL APEX APOLOGY APPEAL APPETITE APPLY APPRECIATE '            
         DC    C'APPROACH APPROVAL AQUEDUCT ARBITRATE ARCH ARCHITEC'            
         DC    C'T AREA ARISE ARITHMETIC ARMOUR AROMA ARREST ARTIFI'            
         DC    C'CIAL ARTISAN ASCEND ASIDE ASLEEP ASPIRE ASSENT ASS'            
         DC    C'IST ASSOCIATION ASSUME ASTERM ASTRAY ASYLUM ATMOSP'            
         DC    C'HERE AUSTERITY AUTHOR AVERAGE AWAKE AXIOMATIC AXE '            
         DC    C'BABY BACKBONE BACKWARDS BAD BAFFLE BAILIFF BAKER B'            
         DC    C'ALLAST BALUSTRADE BANJO BANKRUPTCY BARGAIN BARNACL'            
         DC    C'ES BARONET BASEBALL BASILICA BASSOON BATCH BATTERY'            
         DC    C' BAYONET BAZOOKA BEARD BEARINGS BEAUTIFY BECOME BE'            
         DC    C'DEVIL BEETLE BEFORE BEGINNER BEHAVIOUR BELIEVE BEL'            
         DC    C'ITTLE BELLICOSE BELT BENCH BENEATH BENEFICIAL BESE'            
         DC    C'ECH BESIDES BETROTH BEVEL BIBLIOGRAPHY BIENNIAL BI'            
         DC    C'G BILLET BIRDCAGE BISHOP BLACK BLAME BLATANT BLEED'            
         DC    C' BLESS BLIND BLIZZARD BLOOD BLOSSOM BLOUSE BLUE BL'            
         DC    C'UNDER BLUSH BOARD BOBSLEIGH BODILY BOHEMIAN BOLSHE'            
         DC    C'VIK BOMB BONDAGE BONUS BOOKWORM BOOMERANG BOREDOM '            
         DC    C'BOTHER BOUNDLESS BOWL BOYCOTT BRACELET BRAMBLE BRA'            
         DC    C'NDY BREAKWATER BREASTPLATE BRETHREN BREWARY BRIDE '            
         DC    C'BRIGADIER BRILLIANT BRINK BRITISH BROAD BROADCAST '            
         DC    C'BROKEN BROTHER BRUSH BUBBLE BUCKET BUFFOON BUGLE B'            
         DC    C'ULGE BUNGLE BUNKER BUREAUCRAT BURLESQUE BUSHEL BUS'            
         DC    C'INESS BUTTER BUTTON CABINET CADMIUM CALENDAR CALF '            
         DC    C'CALISTHENICS CALM CAMOUFLAGE CANCEL CANDLE CAMERA '            
         DC    C'CAMPONILE CANDIDATE CANNIBAL CANOPY CAPABILITY CAP'            
         DC    C'ACITY CAPITAL CAPTAIN CARAMEL CARBON CARD CARICATU'            
         DC    C'RE CARPET CARRY CARTILAGE CASCADE CASEMENT CASSERO'            
         DC    C'LE CASTLE CASUALTY CATAPULT CATEGORY CATHOLIC CAVE'            
         DC    C' CELEBRITY CELLO CERAMIC CERTAIN CHAIN CHALLENGE C'            
         DC    C'HANCE CHANNEL CHAPTER CHARACTER CHART CHASSIS CHAU'            
         DC    C'FFEUR CHECKMATE CHEROOT CHILDBIRTH CHIVALRY CHLORO'            
         DC    C'PHYLL CHORISTER CHRONICLE CHUTNEY CINNAMON CIRCULA'            
         DC    C'TION CITATION CLAIRVOYANT CLARITY CLASSICAL CLEAN '            
         DC    C'CLEAVAGE CLIMATE CLOCKWORK CLOISTER CLOSET CLOUDED'            
         DC    C' CLUMSY COALITION COBBLER COCKTAIL COEXIST COIFFUR'            
         DC    C'E COLLAPSE COLLATERAL COLLISION COLONY COLUMNIST C'            
         DC    C'OMBUSTION COMEDY COMFORT COMMAND COMMENTARY COMMOD'            
         DC    C'ITY COMMONER COMMUNICATE COMPANY COMPASS COMPENSAT'            
         DC    C'E COMPLEXION COMPOSER COMPROMISE CONCEAL CONCERT C'            
         DC    C'ONCLUSIVE CONCRETE CONDITION CONFER CONFLICT CONFU'            
         DC    C'SE CONGENITAL CONQUER CONSECUTIVE CONSERVE CONSIDE'            
         DC    C'RATION CONSONANT CONSTITUTION CONSTRUCT CONTEND CO'            
         DC    C'NTINENT CONTRACT CONTRAVENE CONTUSE CONVENTION CON'            
         DC    C'VERT COOL COPIOUS COPYRIGHT CORKSCREW CORPORATE CO'            
         DC    C'RRECT CORRODE COSMOPOLITAN COTTAGE COUNT COUNTERAC'            
         DC    C'T COUNTRY COUPON COURTIER COVER COWARD CRACK CRADL'            
         DC    C'E CRAMP CRAYON CREATURE CREDIBLE CRESCENDO CRIMINA'            
         DC    C'L CRITERION CROCODILE CROOKED CROQUET CROSSING CRU'            
         DC    C'CIBLE CRUSADE CRUSTACEAN CRYSTALLINE CULLINARY CUL'            
         DC    C'TIVATE CURB CURVATURE CUSTOM CUTLERY CYCLE CYNICIS'            
         DC    C'M DAGGER DAMAGE DANCE DANGEROUS DAUGHTER DAWDLE DA'            
         DC    C'YBREAK DAZZLE DEAFNESS DEALER DEBASED DEBONAIR DEC'            
         DC    C'AY DECIPHER DECOMPOSE DEDUCE DEFAULT DEFENCE DEFIC'            
         DC    C'IT DEFUNCT DELEGATE DELICIOUS DEMAND DEMOCRACY DEN'            
         DC    C'IAL DENSE DEPARTURE DEPOSIT DEPRIVE DERELICT DESCE'            
         DC    C'ND DESIGN DESPARATE DESTINY DETERMINE DEVIATE DEVO'            
         DC    C'ID DEXTEROUS DIAPHRAGM DICTATOR DIFFICULT DIGEST D'            
         DC    C'ILEMMA DIMINISH DIPSOMANIAC DIRECTOR DISASTER DISC'            
         DC    C'ERN DISCIPLE DISCORD DISCOVER DISCUSSION DISEMBARK'            
         DC    C' DISHEARTEN DISINTEGRATE DISMAY DISPARITY DISPOSAL'            
         DC    C' DISPUTE DISSECT DISSOLVE DISTANCE DISTINCT DISTRI'            
         DC    C'CT DIVERSITY DIVIDE DIVIDEND DO DOCUMENT DOLLAR DO'            
         DC    C'MICILE DONATION DORMANT DOUBLE DOVETAIL DOWNHILL D'            
         DC    C'RAFT DRAMATIC DRAPERY DRASTIC DREADFUL DRENCH DRIB'            
         DC    C'BLE DRIVER DROMEDARY DRUG DUBIOUS DUCKLING DUEL DU'            
         DC    C'MBFOUNDED DUPLEX DURATION DUTIFUL DYNAMIC EARNEST '            
         DC    C'EARTHQUAKE EBONY EBULLIENT ECHO ECSTATIC EDIFICE E'            
         DC    C'FFICIENT EGOTISM ELASTICITY ELECTRIC ELEPHANT ELIM'            
         DC    C'INATE ELUCIDATE EMBLEM EMBRYONIC EMPLOY BKEND     '            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002GAHNG01   08/22/00'                                      
         END                                                                    
