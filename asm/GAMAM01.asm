*          DATA SET GAMAM01    AT LEVEL 010 AS OF 08/22/00                      
*PHASE TB0D01A                                                                  
         SPACE 1                                                                
*  TABLES OF TYPES,QUESTIONS,ANIMALS,&CHARACTERISTICS                           
         SPACE 1                                                                
         PRINT NOGEN                                                            
TABLES   CSECT                                                                  
Y        EQU   X'02'                                                            
N        EQU   X'01'                                                            
D        EQU   X'80'                                                            
*                                                                               
*                                                                               
         DC    A(TYPTABH-TABLES)                                                
         DC    A(RATTABH-TABLES)                                                
         DC    A(TABLESX-TABLES)                                                
*                                                                               
*                       TYPE TABLES                                             
*                                                                               
TYPTABH  DC    A(L'TYPTAB)                                                      
         DC    A((TYPTABX-TYPTAB)/L'TYPTAB)                                     
         DC    A(TYPQNTAB-TABLES)                                               
         DC    A(TYPES-TABLES)                                                  
         DC    A(TYPTAB-TABLES)                                                 
*                                                                               
TYPTAB   DS    0CL15                                                            
*                                                                               
         DC    AL1(Y,Y,N,N,D,D,Y,N,N,N,N,N,N,N,Y) APES                          
         DC    AL1(Y,N,N,N,D,D,Y,N,N,N,N,N,N,N,N)    APES                       
         DC    AL1(N,D,N,N,N,D,Y,D,N,Y,N,N,N,N,N)   ANTEATERS                   
         DC    AL1(D,Y,N,N,D,Y,Y,D,N,N,N,Y,N,N,N)   BEARS                       
         DC    AL1(D,Y,N,N,Y,Y,Y,Y,N,N,N,N,Y,N,N)   CATS                        
         DC    AL1(N,Y,N,Y,N,N,N,N,N,N,N,N,N,N,N) COWS,DEER,SHEEP,ETC.          
         DC    AL1(N,Y,N,N,Y,Y,Y,Y,Y,N,N,N,N,N,N)   DOGS                        
         DC    AL1(N,D,Y,N,D,N,N,D,N,N,N,N,N,N,N)   SEA MAMMALS                 
         DC    AL1(D,Y,N,N,D,D,Y,D,N,N,N,N,N,Y,N)   FLYING MAMMALS              
         DC    AL1(N,Y,N,N,N,N,N,D,N,N,N,N,N,N,N)   HORSES                      
         DC    AL1(D,Y,N,N,D,D,Y,D,N,N,Y,N,N,N,N)   MARSUPIALS                  
         DC    AL1(N,N,N,N,D,N,Y,D,N,N,N,N,N,N,N)   PACHYDERMS                  
         DC    AL1(D,Y,N,N,N,Y,Y,D,N,N,N,N,N,N,N)   RODENTS                     
         DC    AL1(D,Y,N,N,Y,Y,Y,Y,N,N,N,N,N,N,N)  WEASELS                      
         DC    AL1(N,N,N,N,N,N,N,N,N,N,N,N,N,N,N)  PIGS                         
TYPTABX  DC    X'FF'                                                            
*                                                                               
TYPQNTAB DC    C'*CAN YOU CLIMB TREES?'                                         
         DC    C'*ARE YOU COVERED IN HAIR? BRISTLES DON''T COUNT!'              
         DC    C'*DO YOU HAVE FLIPPERS/FINS?'                                   
         DC    C'*DO YOU HAVE HORNS/ANTLERS?'                                   
         DC    C'*DO YOU EAT MEAT/FISH?'                                        
         DC    C'*DO YOU HAVE PAWS?'                                            
         DC    C'*DO YOU HAVE TOES/CLAWS?'                                      
         DC    C'*DO YOU HAVE WHISKERS?'                                        
         DC    C'*DO YOU HOWL AT THE MOON?'                                     
         DC    C'*WOULD YOU RATHER EAT ANTS?'                                   
         DC    C'*DO YOU HAVE A POUCH?'                                         
         DC    C'*COULD YOU GIVE ME A NASTY HUG?'                               
         DC    C'*IF I SCRATCHED YOUR TUMMY WOULD YOU PURR?'                    
         DC    C'*CAN YOU FLY?'                                                 
         DC    C'*WOULD YOU GO APE FOR A BANANA?'                               
         DC    C'*',X'FF'                                                       
*                                                                               
TYPES    DC    A(APETABH-TABLES)   APES                                         
         DC    A(APETABH-TABLES)                                                
         DC    A(ANTTABH-TABLES)   ANTEATERS                                    
         DC    A(BARTABH-TABLES)   BEARS                                        
         DC    A(CATABH-TABLES)    CATS                                         
         DC    A(CTPTABH-TABLES)   COWS,DEER,SHEEP,ETC,                         
         DC    A(DOGTABH-TABLES)   DOGS                                         
         DC    A(SEATABH-TABLES)   SEA MAMMALS                                  
         DC    A(FLYTABH-TABLES)   FLYING MAMMALS                               
         DC    A(HORTABH-TABLES)   HORSES                                       
         DC    A(MARTABH-TABLES)   MARSUPIALS                                   
         DC    A(PACTABH-TABLES)   PACHYDERMS                                   
         DC    A(RATTABH-TABLES)   RODENTS                                      
         DC    A(WSLTABH-TABLES)   WEASELS                                      
         DC    A(PIGTABH-TABLES)   PIGS                                         
         DC    X'FF'                                                            
*                                                                               
*                       ANTEATER TABLES                                         
*                                                                               
ANTTABH  DC    A(L'ANTTAB)                                                      
         DC    A((ANTTABX-ANTTAB)/L'ANTTAB)                                     
         DC    A(ANTQNTAB-TABLES)                                               
         DC    A(ANTEATRS-TABLES)                                               
         DC    A(ANTTAB-TABLES)                                                 
*                                                                               
ANTTAB   DS    0CL4                                                             
         DC    AL1(N,N,N,N)   AARDVARK                                          
         DC    AL1(Y,Y,N,N)   ARMADILLO                                         
         DC    AL1(Y,N,N,N)   ANTEATER                                          
         DC    AL1(N,Y,N,N)   PANGOLIN                                          
         DC    AL1(N,Y,Y,Y)   HEDGEHOG                                          
         DC    AL1(D,Y,N,Y)   PORCUPINE                                         
ANTTABX  DC    X'FF'                                                            
*                                                                               
*                                                                               
ANTQNTAB DC    C'*DO YOU LIVE IN THE LAND OF THE GAUCHOS,PACHUCHO?'             
         DC    C'*CAN YOU CURL UP INTO A BALL?'                                 
         DC    C'*MIGHT I FIND YOU IN MILTON KEYNES?'                           
         DC    C'*ARE YOU SPINEY?'                                              
         DC    C'*',X'FF'                                                       
*        DC                                                                     
ANTEATRS DC    C'NAMES'                                                         
         DC    C'*AARDVARK'                                                     
         DC    C'*ARMADILLO'                                                    
         DC    C'*ANTEATER'                                                     
         DC    C'*PANGOLIN'                                                     
         DC    C'*HEDGEHOG'                                                     
         DC    C'*PORCUPINE'                                                    
         DC    C'*',X'FF'                                                       
*                                                                               
*                       APE TABLES                                              
*                                                                               
APETABH  DC    A(L'APETAB)                                                      
         DC    A((APETABX-APETAB)/L'APETAB)                                     
         DC    A(APEQNTAB-TABLES)                                               
         DC    A(APES-TABLES)                                                   
         DC    A(APETAB-TABLES)                                                 
*                                                                               
APETAB   DS    0CL8                                                             
         DC    AL1(Y,Y,N,Y,Y,N,N,N)     BABOON                                  
         DC    AL1(N,Y,N,Y,Y,Y,N,N)     CHIMPANZEE                              
         DC    AL1(N,N,N,Y,N,N,N,N)     GIBBON                                  
         DC    AL1(N,Y,N,N,Y,N,N,N)     GORILLA                                 
         DC    AL1(Y,Y,N,Y,N,N,N,N)     GREEN MONKEY                            
         DC    AL1(Y,N,Y,Y,Y,N,N,Y)     HOWLING MONKEY                          
         DC    AL1(Y,Y,N,N,N,N,N,N)     LEMUR                                   
         DC    AL1(N,Y,Y,D,Y,Y,N,D)     MAN                                     
         DC    AL1(N,Y,N,Y,Y,N,N,N)     MANDRILL                                
         DC    AL1(Y,N,Y,Y,N,N,N,N)     MARMOSET                                
         DC    AL1(N,N,N,N,Y,N,N,N)     ORANG UTAN                              
         DC    AL1(Y,N,N,Y,N,N,N,N)     RHESES MONKEY                           
         DC    AL1(Y,N,Y,Y,N,N,Y,N)     SPIDER MONKEY                           
APETABX  DC    X'FF'                                                            
*                                                                               
APEQNTAB DC    C'*DO YOU HAVE A TAIL?'                                          
         DC    C'*DO YOU LIVE IN AFRICA?'                                       
         DC    C'*DO YOU LIVE IN S.AMERICA?'                                    
         DC    C'*DO YOU GO AROUND IN LARGE GROUPS?'                            
         DC    C'*ARE YOU BIGGER THAN AN ALSATIAN?'                             
         DC    C'*DO YOU DRINK PG TIPS?'                                        
         DC    C'*OUGHT YOU TO HAVE EIGHT LEGS?'                                
         DC    C'*DO YOU HOWL?'                                                 
         DC    C'*',X'FF'                                                       
*                                                                               
APES     DC    C'NAMES'                                                         
         DC    C'*BABOON'                                                       
         DC    C'*CHIMPANZEE'                                                   
         DC    C'*GIBBON'                                                       
         DC    C'*GORILLA'                                                      
         DC    C'*GREEN MONKEY'                                                 
         DC    C'*HOWLING MONKEY'                                               
         DC    C'*LEMUR'                                                        
         DC    C'*MAN'                                                          
         DC    C'*MANDRILL'                                                     
         DC    C'*MARMOSET'                                                     
         DC    C'*ORANG UTAN'                                                   
         DC    C'*RHESUS MONKEY'                                                
         DC    C'*SPIDER MONKEY'                                                
         DC    C'*',X'FF'                                                       
*                                                                               
*                       BEAR TABLES                                             
*                                                                               
BARTABH  DC    A(L'BARTAB)                                                      
         DC    A((BARTABX-BARTAB)/L'BARTAB)                                     
         DC    A(BARQNTAB-TABLES)                                               
         DC    A(BEARS-TABLES)                                                  
         DC    A(BARTAB-TABLES)                                                 
*                                                                               
BARTAB   DS    0CL3                                                             
         DC    AL1(N,N,Y)     KOALA BEAR                                        
         DC    AL1(N,Y,N)     BLACK BEAR                                        
         DC    AL1(N,N,N)     BROWN BEAR                                        
         DC    AL1(Y,Y,N)     GRIZZLY BEAR                                      
         DC    AL1(Y,N,N)     POLAR BEAR                                        
BARTABX  DC    X'FF'                                                            
*                                                                               
BARQNTAB DC    C'*ARE YOU NOTORIOUSLY DANGEROUS?'                               
         DC    C'*DO YOU LIVE IN THE ROCKIES?'                                  
         DC    C'*HAVE YOU EVER WALTZED WITH MATILDA?'                          
         DC    C'*',X'FF'                                                       
*                                                                               
BEARS    DC    C'NAMES'                                                         
         DC    C'*KOALA BEAR'                                                   
         DC    C'*BLACK BEAR'                                                   
         DC    C'*BROWN BEAR'                                                   
         DC    C'*GRIZZLY BEAR'                                                 
         DC    C'*POLAR BEAR'                                                   
         DC    C'*',X'FF'                                                       
*                                                                               
*                       CAT TABLES                                              
*                                                                               
CATABH   DC    A(L'CATAB)                                                       
         DC    A((CATABX-CATAB)/L'CATAB)                                        
         DC    A(CATQNTAB-TABLES)                                               
         DC    A(CATS-TABLES)                                                   
         DC    A(CATAB-TABLES)                                                  
*                                                                               
CATAB    DS    0CL11                                                            
         DC    AL1(Y,N,Y,N,N,N,N,Y,N,N,N)    BLACK PANTHER                      
         DC    AL1(N,N,Y,N,N,N,N,N,N,N,D)    BOBCAT                             
         DC    AL1(N,N,N,N,N,N,N,D,N,N,D)    CAT                                
         DC    AL1(Y,Y,N,N,Y,N,N,N,N,N,Y)    CHEETAH                            
         DC    AL1(N,Y,N,N,N,D,N,N,N,N,N)    CIVET                              
         DC    AL1(Y,N,Y,N,N,N,Y,D,N,N,N)    JAGUAR                             
         DC    AL1(Y,Y,N,N,N,N,N,N,N,N,Y)    LEOPARD                            
         DC    AL1(Y,Y,N,N,N,N,N,N,Y,N,N)    LION                               
         DC    AL1(N,N,N,N,N,N,N,N,N,Y,N)    LYNX                               
         DC    AL1(N,N,N,Y,N,N,N,D,N,N,N)    PINE MARTEN                        
         DC    AL1(N,N,N,Y,N,Y,N,D,N,N,N)    POLECAT                            
         DC    AL1(Y,N,Y,N,N,N,N,N,N,N,N)    PUMA                               
         DC    AL1(Y,N,Y,N,N,N,N,N,N,N,Y)    OCELOT                             
         DC    AL1(Y,N,N,N,N,N,N,N,N,N,N)    TIGER                              
CATABX   DC    X'FF'                                                            
*                                                                               
CATQNTAB DC    C'*ARE YOU BIG GAME?'                                            
         DC    C'*ARE YOU AFRICAN?'                                             
         DC    C'*DO YOU LIVE BETWEEN ALASKA AND PATAGONIA?'                    
         DC    C'*COULD I MISTAKE YOU FOR A LARGE WEASEL?'                      
         DC    C'*ARE YOU THE FASTEST THING ON FOUR LEGS?'                      
         DC    C'*DO YOU STINK?'                                                
         DC    C'*ARE YOU MADE IN COVENTRY?'                                    
         DC    C'*ARE YOU BLACK?'                                               
         DC    C'*DID YOUR DAD HAVE A MANE?'                                    
         DC    C'*DOES YOUR NAME END IN ''X''?'                                 
         DC    C'*ARE YOU COVERED IN SPOTS?'                                    
         DC    C'*',X'FF'                                                       
*                                                                               
CATS     DC    C'NAMES'                                                         
         DC    C'*BLACK PANTHER'                                                
         DC    C'*BOBCAT'                                                       
         DC    C'*CAT'                                                          
         DC    C'*CHEETAH'                                                      
         DC    C'*CIVET'                                                        
         DC    C'*JAGUAR'                                                       
         DC    C'*LEOPARD'                                                      
         DC    C'*LION'                                                         
         DC    C'*LYNX'                                                         
         DC    C'*PINE MARTEN'                                                  
         DC    C'*POLECAT'                                                      
         DC    C'*PUMA'                                                         
         DC    C'*OCELOT'                                                       
         DC    C'*TIGER'                                                        
         DC    C'*',X'FF'                                                       
*                                                                               
*                       COW&SHEEP TYPE ANIMALS (FIRST STAGE)                    
*                                                                               
CTPTABH  DC    A(L'CTPTAB)                                                      
         DC    A((CTPTABX-CTPTAB)/L'CTPTAB)                                     
         DC    A(CTPQNTAB-TABLES)                                               
         DC    A(CTPS-TABLES)                                                   
         DC    A(CTPTAB-TABLES)                                                 
*                                                                               
CTPTAB   DS    0CL2                                                             
         DC    AL1(Y,D)       DEER TYPE ANIMALS                                 
         DC    AL1(N,Y)       AFRICAN COWS,SHEEP,ETC                            
         DC    AL1(N,N)       NON-AFRICAN COWS,SHEEP,ETC.                       
CTPTABX  DC    X'FF'                                                            
*                                                                               
CTPQNTAB DC    C'*DO YOUR HORNS HAVE BRANCHES?'                                 
         DC    C'*ARE YOU AN AFRICAN ANIMAL?'                                   
         DC    C'*',X'FF'                                                       
*                                                                               
CTPS     DC    A(DERTABH-TABLES)                                                
         DC    A(AFCTABH-TABLES)                                                
         DC    A(OTCTABH-TABLES)                                                
         DC    X'FF'                                                            
*                                                                               
*                       AFRICAN COWS&SHEEP                                      
*                                                                               
AFCTABH  DC    A(L'AFCTAB)                                                      
         DC    A((AFCTABX-AFCTAB)/L'AFCTAB)                                     
         DC    A(AFCQNTAB-TABLES)                                               
         DC    A(AFRICANS-TABLES)                                               
         DC    A(AFCTAB-TABLES)                                                 
*                                                                               
AFCTAB   DS    0CL8                                                             
         DC    AL1(D,Y,N,D,N,N,N,Y)   GIRAFFE                                   
         DC    AL1(Y,N,N,N,N,Y,N,N)  IBEX                                       
         DC    AL1(Y,Y,N,Y,N,N,N,N)  ANTELOPE                                   
         DC    AL1(Y,Y,N,Y,N,N,Y,N)  GAZELLE                                    
         DC    AL1(Y,Y,N,N,N,N,N,N)  IMPALA                                     
         DC    AL1(Y,Y,N,N,D,Y,N,N)  KUDU                                       
         DC    AL1(Y,N,Y,N,N,N,N,N)  SPRINGBOK                                  
         DC    AL1(N,Y,N,D,N,Y,N,N)  GNU                                        
         DC    AL1(N,N,N,D,N,Y,N,N)  OX                                         
         DC    AL1(N,Y,N,N,Y,Y,N,N)  ZEBU                                       
         DC    AL1(N,Y,N,N,N,N,N,N)  WATER BUFFALO                              
AFCTABX  DC    X'FF'                                                            
*                                                                               
AFCQNTAB DC    C'*DO YOU LOOK MORE LIKE A DEER THAN A BUFFALO?'                 
         DC    C'*DOES YOUR NAME END IN A VOWEL?'                               
         DC    C'*DO YOU PLAY RUGBY?'                                           
         DC    C'*DO YOU COME IN MORE THAN ONE VARIETY?'                        
         DC    C'*DO YOU HAVE A HUMP?'                                          
         DC    C'*SHOULD I SPELL YOU IN LESS THAN FIVE LETTERS?'                
         DC    C'*ARE YOU PROVERBIALLY GRACEFUL?'                               
         DC    C'*IF YOU STUCK YOUR NECK OUT WOULD IT GO A VERY LONG WA*        
               Y?'                                                              
         DC    C'*',X'FF'                                                       
*                                                                               
AFRICANS DC    C'NAMES'                                                         
         DC    C'*GIRAFFE'                                                      
         DC    C'*IBEX'                                                         
         DC    C'*ANTELOPE'                                                     
         DC    C'*GAZELLE'                                                      
         DC    C'*IMPALA'                                                       
         DC    C'*KUDU'                                                         
         DC    C'*SPRINGBOK'                                                    
         DC    C'*GNU'                                                          
         DC    C'*OX'                                                           
         DC    C'*ZEBU'                                                         
         DC    C'*WATER BUFFALO'                                                
         DC    C'*',X'FF'                                                       
*                                                                               
*                       DEER TABLES                                             
*                                                                               
DERTABH  DC    A(L'DERTAB)                                                      
         DC    A((DERTABX-DERTAB)/L'DERTAB)                                     
         DC    A(DERQNTAB-TABLES)                                               
         DC    A(DEER-TABLES)                                                   
         DC    A(DERTAB-TABLES)                                                 
*                                                                               
DERTAB   DS    0CL5                                                             
         DC    AL1(Y,N,N,N,N)      CARIBOU                                      
         DC    AL1(Y,N,Y,N,N)      ELK                                          
         DC    AL1(N,N,Y,N,N)      FALLOW DEER                                  
         DC    AL1(Y,N,N,N,Y)      MOOSE                                        
         DC    AL1(Y,Y,Y,Y,N)      REINDEER                                     
         DC    AL1(N,N,Y,Y,N)      ROE DEER                                     
         DC    AL1(Y,N,Y,Y,N)      RED DEER                                     
DERTABX  DC    X'FF'                                                            
*                                                                               
DERQNTAB DC    C'*ARE YOU AS BIG AS A REINDEER?'                                
         DC    C'*DO YOU HELP DELIVER CHRISTMAS PRESENTS?'                      
         DC    C'*DO SOME OF YOU LIVE IN EUROPE?'                               
         DC    C'*DOES YOUR NAME BEGIN WITH ''R''?'                             
         DC    C'*COULD I TELL WHO YOU ARE JUST BY LOOKING AT YOUR ANTL*        
               ERS?'                                                            
         DC    C'*',X'FF'                                                       
*                                                                               
DEER     DC    C'NAMES'                                                         
         DC    C'*CARIBOU'                                                      
         DC    C'*ELK'                                                          
         DC    C'*FALLOW DEER'                                                  
         DC    C'*MOOSE'                                                        
         DC    C'*REINDEER'                                                     
         DC    C'*ROE DEER'                                                     
         DC    C'*RED DEER'                                                     
         DC    C'*',X'FF'                                                       
*                                                                               
*                       NON-AFRICAN COWS&SHEEP                                  
*                                                                               
OTCTABH  DC    A(L'OTCTAB)                                                      
         DC    A((OTCTABX-OTCTAB)/L'OTCTAB)                                     
         DC    A(OTCQNTAB-TABLES)                                               
         DC    A(OTHERS-TABLES)                                                 
         DC    A(OTCTAB-TABLES)                                                 
*                                                                               
OTCTAB   DS    0CL10                                                            
         DC    AL1(D,Y,N,Y,N,N,N,N,Y,N)      ARGALI                             
         DC    AL1(Y,Y,Y,Y,N,N,N,N,N,N)      BIGHORN                            
         DC    AL1(Y,D,N,Y,N,Y,N,N,N,N)      CHAMOIS                            
         DC    AL1(N,D,N,N,Y,Y,N,N,N,N)      GOAT                               
         DC    AL1(N,Y,Y,Y,Y,N,N,N,Y,N)      LLAMA                              
         DC    AL1(N,Y,N,N,Y,N,N,N,N,N)      SHEEP                              
         DC    AL1(D,D,Y,Y,N,Y,N,N,N,N)      MOUNTAIN GOAT                      
         DC    AL1(N,N,Y,N,N,N,Y,N,N,N)      BISON                              
         DC    AL1(N,N,N,N,Y,N,N,N,N,Y)      OX                                 
         DC    AL1(N,N,N,N,Y,N,N,N,N,N)      COW                                
         DC    AL1(N,N,Y,N,N,N,N,N,N,N)      MUSK OX                            
         DC    AL1(N,N,N,D,Y,N,N,Y,N,N)      YAK                                
         DC    AL1(N,N,N,N,Y,N,N,N,Y,N)      ZEBU                               
         DC    AL1(Y,N,N,N,Y,N,N,N,N,N)      WATER BUFFALO                      
OTCTABX  DC    X'FF'                                                            
*                                                                               
OTCQNTAB DC    C'*ARE YOU GAME?'                                                
         DC    C'*DO YOU HAVE A FLEECE?'                                        
         DC    C'*DO YOU LIVE BETWEEN ALASKA AND PATAGONIA?'                    
         DC    C'*DO YOU CLIMB MOUNTAINS?'                                      
         DC    C'*ARE YOU A KEPT MAMMAL,A DOMESTIC TYPE?'                       
         DC    C'*ARE YOU SOME KIND OF GOAT?'                                   
         DC    C'*ARE YOU SITTING BULL''S FAVOURITE LUNCH?'                     
         DC    C'*ARE YOU A FRIEND OF (TIBETAN) LAMAS?'                         
         DC    C'*DOES YOUR NAME END IN A VOWEL?'                               
         DC    C'*ARE YOU PROVERBIALLY STRONG?'                                 
         DC    C'*',X'FF'                                                       
*                                                                               
OTHERS   DC    C'NAMES'                                                         
         DC    C'*ARGALI'                                                       
         DC    C'*BIGHORN'                                                      
         DC    C'*CHAMOIS'                                                      
         DC    C'*GOAT'                                                         
         DC    C'*LLAMA'                                                        
         DC    C'*SHEEP'                                                        
         DC    C'*MOUNTAIN GOAT'                                                
         DC    C'*BISON'                                                        
         DC    C'*OX'                                                           
         DC    C'*COW'                                                          
         DC    C'*MUSK OX'                                                      
         DC    C'*YAK'                                                          
         DC    C'*ZEBU'                                                         
         DC    C'*WATER BUFFALO'                                                
         DC    C'*',X'FF'                                                       
*                                                                               
*                       DOG TABLES                                              
DOGTABH  DC    A(L'DOGTAB)                                                      
         DC    A((DOGTABX-DOGTAB)/L'DOGTAB)                                     
         DC    A(DOGQNTAB-TABLES)                                               
         DC    A(DOGS-TABLES)                                                   
         DC    A(DOGTAB-TABLES)                                                 
*                                                                               
DOGTAB   DS    0CL8                                                             
         DC    AL1(N,N,N,N,Y,N,N,N)     WOLF                                    
         DC    AL1(N,N,N,N,N,N,Y,D)     DOG                                     
         DC    AL1(Y,N,N,N,Y,N,N,Y)     HYENA                                   
         DC    AL1(N,N,N,N,Y,N,N,Y)     JACKAL                                  
         DC    AL1(N,Y,N,N,N,N,N,Y)     DINGO                                   
         DC    AL1(N,N,N,N,N,Y,N,N)     FOX                                     
         DC    AL1(N,N,N,Y,D,N,N,Y)     COYOTE                                  
         DC    AL1(N,N,Y,N,N,N,N,N)     ARTIC FOX                               
         DC    AL1(N,N,N,N,N,N,N,N)     WOLVERINE                               
DOGTABX  DC    X'FF'                                                            
*                                                                               
DOGQNTAB DC    C'*DO YOU LAUGH?'                                                
         DC    C'*DO YOU LIVE DOWN UNDER?'                                      
         DC    C'*ARE YOU WHITE?'                                               
         DC    C'*DO YOU CHASE ROADRUNNERS?'                                    
         DC    C'*DO YOU LIVE IN LARGE PACKS?'                                  
         DC    C'*DO YOU LIVE IN AN EARTH?'                                     
         DC    C'*ARE YOU A KEPT MAMMAL,A DOMESTIC TYPE?'                       
         DC    C'*DO YOU LIVE IN A HOT CLIMATE?'                                
         DC    C'*',X'FF'                                                       
*                                                                               
DOGS     DC    C'NAMES'                                                         
         DC    C'*WOLF'                                                         
         DC    C'*DOG'                                                          
         DC    C'*HYENA'                                                        
         DC    C'*JACKAL'                                                       
         DC    C'*DINGO'                                                        
         DC    C'*FOX'                                                          
         DC    C'*COYOTE'                                                       
         DC    C'*ARCTIC FOX'                                                   
         DC    C'*WOLVERINE'                                                    
         DC    C'*',X'FF'                                                       
*                                                                               
*                       FLIER TABLES                                            
*                                                                               
FLYTABH  DC    A(L'FLYTAB)                                                      
         DC    A((FLYTABX-FLYTAB)/L'FLYTAB)                                     
         DC    A(FLYQNTAB-TABLES)                                               
         DC    A(FLIERS-TABLES)                                                 
         DC    A(FLYTAB-TABLES)                                                 
*                                                                               
FLYTAB   DS    0CL2                                                             
         DC    AL1(Y,N)  FLYING SQUIRREL                                        
         DC    AL1(N,N)  BAT                                                    
         DC    AL1(N,Y)  VAMPIRE BAT                                            
FLYTABX  DC    X'FF'                                                            
*                                                                               
FLYQNTAB DC    C'*DO YOU HAVE A TAIL?'                                          
         DC    C'*DO YOU COME FROM TRANSYLVANIA?'                               
         DC    C'*',X'FF'                                                       
*                                                                               
FLIERS   DC    C'NAMES'                                                         
         DC    C'*FLYING SQUIRREL'                                              
         DC    C'*BAT'                                                          
         DC    C'*VAMPIRE BAT'                                                  
         DC    C'*',X'FF'                                                       
*                                                                               
*                       HORSE TABLES                                            
*                                                                               
HORTABH  DC    A(L'HORTAB)                                                      
         DC    A((HORTABX-HORTAB)/L'HORTAB)                                     
         DC    A(HORQNTAB-TABLES)                                               
         DC    A(HORSES-TABLES)                                                 
         DC    A(HORTAB-TABLES)                                                 
*                                                                               
HORTAB   DS    0CL8                                                             
         DC    AL1(N,Y,N,N,N,Y,N,N)     ASS                                     
         DC    AL1(N,Y,N,N,N,Y,N,Y)     DONKEY                                  
         DC    AL1(N,N,N,Y,Y,Y,N,N)     CAMEL                                   
         DC    AL1(N,N,N,Y,N,Y,N,N)     DROMEDARY                               
         DC    AL1(N,N,N,N,N,Y,N,N)     HORSE                                   
         DC    AL1(N,N,Y,N,N,Y,N,N)     LLAMA                                   
         DC    AL1(N,Y,N,N,N,Y,Y,N)     MULE                                    
         DC    AL1(N,N,N,N,N,N,N,N)     ONAGER                                  
         DC    AL1(Y,N,N,N,N,N,N,N)     ZEBRA                                   
         DC    AL1(N,N,Y,N,N,N,N,N)     TAPIR                                   
HORTABX  DC    X'FF'                                                            
*                                                                               
HORQNTAB DC    C'*DO YOU HAVE STRIPES?'                                         
         DC    C'*DO YOU BRAY?'                                                 
         DC    C'*DO YOU LIVE IN S.AMERICA?'                                    
         DC    C'*IF YOU RAN OUT OF WATER WOULD YOU LIVE ON YOUR BACK?'         
         DC    C'*DO YOU HAVE TWO HUMPS?'                                       
         DC    C'*ARE YOU A DOMESTIC ANIMAL?'                                   
         DC    C'*WOULD I BE VERY SURPRISED TO FIND YOU HAD CHILDREN?'          
         DC    C'*DOES YOUR NAME RHYME WITH WONKY?'                             
         DC    C'*',X'FF'                                                       
*                                                                               
HORSES   DC    C'NAMES'                                                         
         DC    C'*ASS'                                                          
         DC    C'*DONKEY'                                                       
         DC    C'*CAMEL'                                                        
         DC    C'*DROMEDARY'                                                    
         DC    C'*HORSE'                                                        
         DC    C'*LLAMA'                                                        
         DC    C'*MULE'                                                         
         DC    C'*ONAGER'                                                       
         DC    C'*ZEBRA'                                                        
         DC    C'*TAPIR'                                                        
         DC    C'*',X'FF'                                                       
*                       MARSUPIAL TABLES                                        
*                                                                               
MARTABH  DC    A(L'MARTAB)                                                      
         DC    A((MARTABX-MARTAB)/L'MARTAB)                                     
         DC    A(MARQNTAB-TABLES)                                               
         DC    A(MARSUPLS-TABLES)                                               
         DC    A(MARTAB-TABLES)                                                 
*                                                                               
MARTAB   DS    0CL7                                                             
         DC    AL1(Y,N,Y,N,N,N,N)       BANDICOOT                               
         DC    AL1(Y,N,N,Y,N,N,N)       KANGAROO                                
         DC    AL1(Y,N,Y,Y,N,N,N)       WALLABEE                                
         DC    AL1(N,Y,Y,N,N,N,N)       KOALA                                   
         DC    AL1(Y,N,Y,N,N,N,Y)       PLATYPUS                                
         DC    AL1(Y,N,Y,N,Y,N,N)       OPOSSUM                                 
         DC    AL1(N,N,Y,N,N,N,N)       WOMBAT                                  
         DC    AL1(Y,N,Y,N,N,Y,N)       TASMANIAN DEVIL                         
MARTABX  DC    X'FF'                                                            
*                                                                               
MARQNTAB DC    C'*DO YOU HAVE A LONGISH TAIL?'                                  
         DC    C'*DO YOU LIVE IN A GUM TREE?'                                   
         DC    C'*ARE YOU SMALLER THAN A KANGAROO?'                             
         DC    C'*DO YOU HOP?'                                                  
         DC    C'*WOULD YOU PLAY DEAD?'                                         
         DC    C'*ARE YOU A BIT OF A DEVIL?'                                    
         DC    C'*DO YOU LAY EGGS?'                                             
         DC    C'*',X'FF'                                                       
*                                                                               
MARSUPLS DC    C'NAMES'                                                         
         DC    C'*BANDICOOT'                                                    
         DC    C'*KANGAROO'                                                     
         DC    C'*WALLABY'                                                      
         DC    C'*KOALA'                                                        
         DC    C'*PLATYPUS'                                                     
         DC    C'*OPOSSUM'                                                      
         DC    C'*WOMBAT'                                                       
         DC    C'*TASMANIAN DEVIL'                                              
         DC    C'*',X'FF'                                                       
*                                                                               
*                       PACHYDERM TABLES                                        
*                                                                               
PACTABH  DC    A(L'PACTAB)                                                      
         DC    A((PACTABX-PACTAB)/L'PACTAB)                                     
         DC    A(PACQNTAB-TABLES)                                               
         DC    A(PACDERMS-TABLES)                                               
         DC    A(PACTAB-TABLES)                                                 
*                                                                               
PACTAB   DS    0CL6                                                             
         DC    AL1(N,Y,D,Y,N,N)    INDIAN ELEPHANT                              
         DC    AL1(Y,Y,D,Y,N,N)    AFRICAN ELEPHANT                             
         DC    AL1(Y,N,N,Y,N,N)    RHINOCEROUS                                  
         DC    AL1(Y,N,Y,Y,N,N)    HIPPOPOTAMOUS                                
         DC    AL1(N,N,N,N,N,N)    ARMADILLO                                    
         DC    AL1(Y,N,N,N,N,N)    PANGOLIN                                     
         DC    AL1(N,N,N,N,Y,Y)    HEDGEHOG                                     
         DC    AL1(D,N,N,N,Y,N)    PORCUPINE                                    
PACTABX  DC    X'FF'                                                            
*                                                                               
PACQNTAB DC    C'*DO YOU LIVE IN AFRICA?'                                       
         DC    C'*IF YOU LEFT FOR THE COAST WOULD YOU PACK YOUR TRUNK?'         
         DC    C'*DO YOU WALLOW?'                                               
         DC    C'*DO YOU GRAZE?'                                                
         DC    C'*ARE YOU A SPIKEY CHARACTER?'                                  
         DC    C'*DO YOU LIVE IN ENGLAND?'                                      
         DC    C'*',X'FF'                                                       
*                                                                               
PACDERMS DC    C'NAMES'                                                         
         DC    C'*INDIAN ELEPHANT'                                              
         DC    C'*AFRICAN ELEPHANT'                                             
         DC    C'*RHINOCEROUS'                                                  
         DC    C'*HIPPOPOTAMUS'                                                 
         DC    C'*ARMADILLO'                                                    
         DC    C'*PANGOLIN'                                                     
         DC    C'*HEDGEHOG'                                                     
         DC    C'*PORCUPINE'                                                    
         DC    C'*',X'FF'                                                       
*                                                                               
*                       PIG TABLES                                              
*                                                                               
PIGTABH  DC    A(L'PIGTAB)                                                      
         DC    A((PIGTABX-PIGTAB)/L'PIGTAB)                                     
         DC    A(PIGQNTAB-TABLES)                                               
         DC    A(PIGS-TABLES)                                                   
         DC    A(PIGTAB-TABLES)                                                 
*                                                                               
PIGTAB   DS    0CL4                                                             
         DC    AL1(Y,N,Y,N)   BOAR                                              
         DC    AL1(N,N,Y,N)   BUSH PIG                                          
         DC    AL1(N,N,N,N)   PECCARY                                           
         DC    AL1(Y,N,N,N)   PIG                                               
         DC    AL1(N,Y,N,N)   TAPIR                                             
         DC    AL1(N,N,Y,Y)   WARTGOG                                           
PIGTABX  DC    X'FF'                                                            
*                                                                               
PIGQNTAB DC    C'*WOULD I FIND YOU IN EUROPE?'                                  
         DC    C'*ARE YOU A CLOSE RELATIVE OF THE HORSE?'                       
         DC    C'*DO YOU HAVE TUSKS?'                                           
         DC    C'*ARE YOU WARTY?'                                               
         DC    C'*',X'FF'                                                       
*                                                                               
PIGS     DC    C'NAMES'                                                         
         DC    C'*BOAR'                                                         
         DC    C'*BUSH PIG'                                                     
         DC    C'*PECCARY'                                                      
         DC    C'*PIG'                                                          
         DC    C'*TAPIR'                                                        
         DC    C'*WARTHOG'                                                      
         DC    C'*',X'FF'                                                       
*                                                                               
*                       RODENT TABLES                                           
*                                                                               
RATTABH  DC    A(L'RATTAB)                                                      
         DC    A((RATTABX-RATTAB)/L'RATTAB)                                     
         DC    A(RATQNTAB-TABLES)                                               
         DC    A(RATS-TABLES)                                                   
         DC    A(RATTAB-TABLES)                                                 
*                                                                               
RATTAB   DS    0CL13                                                            
         DC    AL1(N,Y,N,N,N,Y,N,N,N,N,N,N,N)  BANDICOOT                        
         DC    AL1(N,N,N,Y,N,Y,N,N,N,N,N,N,N)  CHINCILLA                        
         DC    AL1(N,Y,N,Y,Y,Y,N,N,N,N,N,N,N)  CHIPMUNK                         
         DC    AL1(Y,N,N,N,N,Y,N,N,N,N,N,N,N)  GERBIL                           
         DC    AL1(Y,N,N,N,N,N,N,N,N,N,N,N,N)  GUINEA PIG                       
         DC    AL1(Y,N,Y,N,N,N,N,N,N,N,N,N,N)  HAMSTER                          
         DC    AL1(N,Y,Y,N,N,N,N,N,N,N,Y,N,N)  HARE                             
         DC    AL1(Y,N,Y,N,N,Y,N,N,N,N,Y,N,N)  DORMOUSE                         
         DC    AL1(Y,N,Y,N,N,Y,N,N,N,N,N,N,N)  MOUSE                            
         DC    AL1(N,N,Y,N,N,N,Y,N,N,N,N,Y,N)  MOLE                             
         DC    AL1(N,Y,N,Y,N,D,N,Y,N,N,N,N,N)  MUSKRAT                          
         DC    AL1(N,Y,Y,N,N,Y,N,N,N,N,N,N,N)  MARMOT                           
         DC    AL1(N,N,N,N,N,N,N,N,N,N,N,N,N)  LEMMING                          
         DC    AL1(N,Y,N,Y,N,Y,N,N,N,N,N,N,N)  PRAIRIE DOG                      
         DC    AL1(Y,Y,Y,N,N,N,N,N,N,N,Y,N,N)  RABBIT                           
         DC    AL1(Y,N,Y,N,N,Y,N,N,N,Y,N,N,N)  RAT                              
         DC    AL1(N,Y,N,N,N,N,N,Y,N,N,N,N,N)  QUOIPU                           
         DC    AL1(N,N,Y,N,N,Y,N,N,N,N,N,N,N)  SHREW                            
         DC    AL1(N,Y,Y,N,Y,Y,N,N,N,N,N,N,N)  SQUIRREL                         
         DC    AL1(N,Y,N,N,Y,N,N,N,N,N,N,N,N)  SLOTH                            
         DC    AL1(N,N,Y,N,N,Y,N,Y,N,N,N,N,N)  VOLE                             
         DC    AL1(N,Y,N,Y,N,Y,N,N,Y,N,N,N,N)  WOODCHUCK                        
         DC    AL1(N,N,Y,N,N,Y,N,Y,N,N,N,Y,N)  WATER RAT                        
         DC    AL1(N,Y,N,N,N,N,N,N,N,N,N,N,N)  PANDA                            
         DC    AL1(N,Y,N,N,N,D,N,Y,N,N,N,N,Y)  PLATYPUS                         
RATTABX  DC    X'FF'                                                            
*                                                                               
RATQNTAB DC    C'*COULD I BUY YOU IN A PET SHOP?'                               
         DC    C'*ARE YOU BIGGER THAN A RAT?'                                   
         DC    C'*DO YOU LIVE IN THE EEC?'                                      
         DC    C'*ARE YOU ONLY FOUND IN THE AMERICAS?'                          
         DC    C'*DO YOU LIVE IN A TREE?'                                       
         DC    C'*HAVE YOU GOT A LOT (OF TAIL) TO TUCK INTO YOUR TROUSE*        
               RS?'                                                             
         DC    C'*ARE YOU BLIND?'                                               
         DC    C'*ARE YOU AQUATIC?'                                             
         DC    C'*IF ONLY WOODCHUCKS CAN CHUCK WOOD,CAN YOU?'                   
         DC    C'*WERE YOU A VICTIM OF THE PIED PIPER?'                         
         DC    C'*ARE YOU SOMETHING OUT OF ALICE IN WONDERLAND?'                
         DC    C'*ARE YOU SOMETHING OUT OF WIND IN THE WILLOWS?'                
         DC    C'*DO YOU LAY EGGS?'                                             
         DC    C'*',X'FF'                                                       
*                                                                               
RATS     DC    C'NAMES'                                                         
         DC    C'*BANDICOOT'                                                    
         DC    C'*CHINCILLA'                                                    
         DC    C'*CHIPMUNK'                                                     
         DC    C'*GERBIL'                                                       
         DC    C'*GUINEA PIG'                                                   
         DC    C'*HAMSTER'                                                      
         DC    C'*HARE'                                                         
         DC    C'*DORMOUSE'                                                     
         DC    C'*MOUSE'                                                        
         DC    C'*MOLE'                                                         
         DC    C'*MUSKRAT'                                                      
         DC    C'*MARMOT'                                                       
         DC    C'*LEMMING'                                                      
         DC    C'*PRAIRIE DOG'                                                  
         DC    C'*RABBIT'                                                       
         DC    C'*RAT'                                                          
         DC    C'*QUOIPU'                                                       
         DC    C'*SHREW'                                                        
         DC    C'*SQUIRREL'                                                     
         DC    C'*SLOTH'                                                        
         DC    C'*VOLE'                                                         
         DC    C'*WOODCHUCK'                                                    
         DC    C'*WATER RAT'                                                    
         DC    C'*PANDA'                                                        
         DC    C'*PLATYPUS'                                                     
         DC    C'*',X'FF'                                                       
*                                                                               
*                       WEASEL TABLES                                           
*                                                                               
WSLTABH  DC    A(L'WSLTAB)                                                      
         DC    A((WSLTABX-WSLTAB)/L'WSLTAB)                                     
         DC    A(WSLQNTAB-TABLES)                                               
         DC    A(WEASELS-TABLES)                                                
         DC    A(WSLTAB-TABLES)                                                 
*                                                                               
WSLTAB   DS    0CL10                                                            
         DC    AL1(N,N,N,Y,N,Y,N,N,N,N)      BADGER                             
         DC    AL1(N,Y,Y,N,N,N,N,N,N,N)      BEAVER                             
         DC    AL1(N,N,N,N,Y,N,N,N,N,N)      MONGOOSE                           
         DC    AL1(N,N,Y,Y,N,N,N,N,N,N)      MINK                               
         DC    AL1(N,N,Y,N,N,N,N,N,N,N)      RACCOON                            
         DC    AL1(Y,N,Y,N,N,N,N,N,N,N)      SKUNK                              
         DC    AL1(Y,N,N,Y,N,N,N,N,N,N)      POLECAT                            
         DC    AL1(N,N,N,Y,N,N,N,N,Y,N)      PINEMARTEN                         
         DC    AL1(Y,Y,Y,N,N,N,N,N,N,N)      MUSKRAT                            
         DC    AL1(N,N,N,Y,N,N,N,N,N,N)      STOAT                              
         DC    AL1(N,N,Y,N,N,N,N,Y,N,N)      WOLVERINE                          
         DC    AL1(N,N,N,Y,N,N,Y,N,N,N)      WEASEL                             
         DC    AL1(N,N,N,Y,N,N,N,N,N,Y)      FERRET                             
         DC    AL1(N,Y,Y,Y,N,N,N,N,N,N)      OTTER                              
WSLTABX  DC    X'FF'                                                            
*                                                                               
WSLQNTAB DC    C'*ARE YOU SMELLY?'                                              
         DC    C'*ARE YOU AQUATIC?'                                             
         DC    C'*MIGHT I FIND YOU IN N.AMERICA?'                               
         DC    C'*MIGHT I FIND YOU IN EUROPE?'                                  
         DC    C'*DO YOU CATCH SNAKES?'                                         
         DC    C'*DO YOU LIVE IN A SET?'                                        
         DC    C'*WOULD YOU WEASEL YOUR WAY INTO MY AFFECTIONS?'                
         DC    C'*WOLVERINE YOU BITE MY ANKLES?'                                
         DC    C'*DO YOU LIVE IN A TREE?'                                       
         DC    C'*HAVE I FERRETED OUT YOUR TRUE IDENTITY?'                      
         DC    C'*',X'FF'                                                       
*                                                                               
WEASELS  DC    C'NAMES'                                                         
         DC    C'*BADGER'                                                       
         DC    C'*BEAVER'                                                       
         DC    C'*MONGOOSE'                                                     
         DC    C'*MINK'                                                         
         DC    C'*RACCOON'                                                      
         DC    C'*SKUNK'                                                        
         DC    C'*POLECAT'                                                      
         DC    C'*PINEMARTEN'                                                   
         DC    C'*MUSKRAT'                                                      
         DC    C'*STOAT'                                                        
         DC    C'*WOLVERINE'                                                    
         DC    C'*WEASEL'                                                       
         DC    C'*FERRET'                                                       
         DC    C'*OTTER'                                                        
         DC    C'*',X'FF'                                                       
*                                                                               
*                       WHALE & SEAL TABLES                                     
*                                                                               
SEATABH  DC    A(L'SEATAB)                                                      
         DC    A((SEATABX-SEATAB)/L'SEATAB)                                     
         DC    A(SEAQNTAB-TABLES)                                               
         DC    A(SEAMAMS-TABLES)                                                
         DC    A(SEATAB-TABLES)                                                 
*                                                                               
SEATAB   DS    0CL10                                                            
         DC    AL1(D,Y,Y,N,N,N,N,D,D,N)      DOLPHIN                            
         DC    AL1(Y,N,N,N,N,N,N,D,N,N)      KILLER WHALE                       
         DC    AL1(Y,D,N,N,Y,N,N,N,D,N)      NARWAHL                            
         DC    AL1(D,Y,N,N,N,N,N,D,D,N)      PORPOISE                           
         DC    AL1(Y,D,N,N,N,N,N,N,N,Y)      WHALE                              
         DC    AL1(N,D,N,Y,N,N,N,N,Y,N)      GREY SEAL                          
         DC    AL1(N,N,N,Y,Y,N,N,N,D,N)      WALRUS                             
         DC    AL1(N,N,N,Y,N,N,Y,N,D,N)      ELEPHANT SEAL                      
         DC    AL1(N,Y,N,N,N,N,N,N,D,N)      SEA OTTER                          
         DC    AL1(N,N,N,Y,N,N,N,Y,D,N)      SEA LION                           
         DC    AL1(N,Y,N,Y,N,N,N,N,N,N)      SEAL                               
         DC    AL1(N,N,N,D,N,Y,N,N,D,N)      MANATEE                            
SEATABX  DC    X'FF'                                                            
*                                                                               
SEAQNTAB DC    C'*DO YOU HAVE A BLOW HOLE?'                                     
         DC    C'*DO YOU LIVE IN EUROPE?'                                       
         DC    C'*ARE YOU A COUSIN OF FLIPPER?'                                 
         DC    C'*CAN YOU CLAP?'                                                
         DC    C'*DO YOU HAVE TUSKS OR A HORN?'                                 
         DC    C'*DID SAILORS THINK YOU WERE A MERMAID?'                        
         DC    C'*DO YOU HAVE AN INFLATABLE CONK?'                              
         DC    C'*DO YOU HAVE A REPUTATION FOR BEING CLEVER WITH BALLS?*        
               '                                                                
         DC    C'*ARE YOU GREY?'                                                
         DC    C'*DO YOU SING?'                                                 
         DC    C'*',X'FF'                                                       
*                                                                               
SEAMAMS  DC    C'NAMES'                                                         
         DC    C'*DOLPHIN'                                                      
         DC    C'*KILLER WHALE'                                                 
         DC    C'*NARWAHL'                                                      
         DC    C'*PORPOISE'                                                     
         DC    C'*WHALE'                                                        
         DC    C'*GREY SEAL'                                                    
         DC    C'*WALRUS'                                                       
         DC    C'*ELEPHANT SEAL'                                                
         DC    C'*SEA OTTER'                                                    
         DC    C'*SEA LION'                                                     
         DC    C'*SEAL'                                                         
         DC    C'*MANATEE'                                                      
         DC    C'*',X'FF'                                                       
*                                                                               
TABLESX  DS    0D                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010GAMAM01   08/22/00'                                      
         END                                                                    
