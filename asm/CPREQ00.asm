*          DATA SET CPREQ00    AT LEVEL 029 AS OF 09/01/00                      
*PHASE TC0400A                                                                  
         TITLE 'CPREQ00 - REQUEST - ROOT CONTROLLER'                            
         PRINT NOGEN                                                            
TC0400   CSECT                                                                  
         NMOD1 1104,TC0400         W/S=80D AND I/O=1024D                        
         LR    R9,RC                                                            
         USING REQTEMP,R9          R9=A(W/S)                                    
         ST    R9,ATEMP                                                         
         EJECT                                                                  
INIT     LM    R2,R3,0(R1)                                                      
         USING REQSAVE,R3          R3=A(TWA)                                    
         ST    R1,APARM                                                         
         ST    R3,ASAVE                                                         
         ST    RB,ABASE                                                         
*                                                                               
         LR    R6,R3               STORE A(FIRST INPUT FLD)                     
         AH    R6,0(R2)                                                         
         ST    R6,AFIRSTF                                                       
         LR    R6,R3               STORE A(LAST INPUT FLD)                      
         AH    R6,2(R2)                                                         
         ST    R6,ALASTF                                                        
         MVC   COUNTF,4(R2)        STORE INPUT FLD COUNT                        
*                                                                               
         MVI   DDS,0                                                            
         CLI   1(R3),C'*'          SET DDS TERMINAL                             
         BNE   *+8                                                              
         MVI   DDS,1                                                            
         MVC   AGYB,0(R1)          STORE AGY BINARY                             
         MVC   AGY,14(R3)          STORE AGY ALPHA                              
         L     R4,16(R1)                                                        
         MVC   FACLIST,0(R4)       STORE COMMON FACILITY LIST                   
*                                                                               
         LA    R6,REQTBL           RELOCATE LOCAL ROUTINES                      
         ST    R6,AREQTBL                                                       
         LA    R6,OLAY                                                          
         ST    R6,AOLAY                                                         
         LA    R6,READFIL                                                       
         ST    R6,ARFIL                                                         
         LA    R6,INITV                                                         
         ST    R6,AINITV                                                        
         LA    R6,RJN                                                           
         ST    R6,ARJN                                                          
*                                                                               
         ST    RB,LOLAYNUM         INIT LAST OLAY NUM & ADR                     
         LA    R6,FILWORK                                                       
         ST    R6,DMCB+16                                                       
         EJECT                                                                  
         LR    R2,R3               R2=A(TWA SCREEN DATA)                        
         USING TC04FFD,R2                                                       
CONTROL  CLI   STATUS,0                                                         
         BE    STATUS0                                                          
         CLI   STATUS,1                                                         
         BE    STATUS1                                                          
         CLI   STATUS,2                                                         
         BE    STATUS2                                                          
         CLI   STATUS,3                                                         
         BE    STATUS3                                                          
         CLI   STATUS,4                                                         
         BE    STATUS4                                                          
         DC    H'0'                                                             
         SPACE 2                                                                
STATUS0  MVI   OLAYNUM,01          INPUT IS REQ DEFN                            
         GOTO1 AOLAY               VALIDATE & BUILD SCREEN                      
         CLI   FERN,X'FF'                                                       
         BL    OERRMSG                                                          
         B     CONTROL                                                          
         SPACE 2                                                                
STATUS1  CLI   REQACTN,C'D'                                                     
         BE    STATUS1A                                                         
         XC    BVRHDR,BVRHDR                                                    
         CLI   REQACTN,C'A'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(23),=C'ENTER REQUEST AMENDMENT'                           
         B     STATUS1A                                                         
         CLI   REQACTN,C'N'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(18),=C'ENTER REQUEST DATA'                                
         B     STATUS1A                                                         
STATUS1A MVI   STATUS,2                                                         
         LA    R5,BVRFRSTH         POSN CURSOR                                  
         SR    R6,R6                                                            
         TM    1(R5),X'20'                                                      
         BZ    *+14                                                             
         IC    R6,0(R5)                                                         
         AR    R5,R6                                                            
         B     *-14                                                             
         ST    R5,FADR                                                          
         B     OHDR                                                             
         SPACE 2                                                                
STATUS2  MVC   REQNUM(18),LREQNUM  INPUT IS REQUEST DATA                        
         MVC   KEY,LKEY                                                         
         MVC   REQREC(106),LREQREC                                              
         LA    R6,BVRFRSTH                                                      
         C     R6,AFIRSTF          WAS A REQ DEFN FLD INPUT                     
         BL    *+12                NO                                           
         MVI   STATUS,0            YES BACK TO STATUS 0                         
         B     CONTROL                                                          
         CLI   REQACTN,C'D'                                                     
         BE    UPDATE                                                           
         B     VALIDATE                                                         
         SPACE 2                                                                
STATUS3  B     UPDATE              DISPLAY/UPDATE DATA                          
         SPACE 2                                                                
STATUS4  XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(22),=C'REQUEST MENU DISPLAYED'                            
         B     UPDATE1                                                          
         EJECT                                                                  
VALIDATE L     R4,AREQTBL                                                       
         AH    R4,REQNDX1          R4=A(REQ TBL FLD ENTRY)                      
         LA    RA,LREQMAP-3        RA=A(REQ MAP TBL ENTRY)                      
         B     VAL5                                                             
         SPACE 2                                                                
VAL1     CLI   0(R4),0             END OF REQ TBL                               
         BE    VALX                YES                                          
         CLI   0(R4),127           COMMENT ENTRY IN REQ TBL                     
         BNL   VAL7                YES                                          
         TM    1(R4),X'80'         DDS ONLY FIELD                               
         BZ    *+12                NO                                           
         TM    REQFMT,X'10'        YES ONLY IF 1UP/2UP                          
         BZ    VAL7                                                             
         CLC   0(1,RA),0(R4)                                                    
         BE    *+6                                                              
         DC    H'0'                DUMP IF TBLS OUT OF STEP                     
         SPACE 2                                                                
         LA    R6,FLDNUMT          FIND REQ FLD NUM IN TBL                      
VAL2     CLI   0(R6),0             R6=A(FLD NUM TBL ENTRY)                      
         BNE   *+6                                                              
         DC    H'0'                DUMP IF REQ NUM NOT IN TBL                   
         CLC   0(1,R6),0(R4)                                                    
         BE    VAL3                                                             
         LA    R6,4(R6)                                                         
         B     VAL2                                                             
         SPACE 2                                                                
VAL3     MVC   OLAYNUM,1(R6)       SET OVERLAY REQD                             
         MVC   ROUTNUM,2(R6)       SET VALIDATION ROUT REQD                     
         MVC   ROUTSUB,3(R6)       SET SUB FLD NUM                              
         MVC   FLDCMNT,2(R4)       SET INPUT FLD COMMENT NUMBER                 
         ST    R5,FLDHADR          SET INPUT FLD HDR ADR                        
         GOTO1 AOLAY               PASS CONTROL TO OVERLAY                      
         TM    FIND,X'01'                                                       
         BZ    *+16                                                             
         TM    FIND,X'FE'                                                       
         BZ    OERRMSG             FLD INPUT INVALID                            
         B     VAL4                FLD INPUT VALID                              
         TM    1(R4),X'01'         FLD NOT INPUT                                
         BZ    OERRMSG             AND IS NOT OPTIONAL                          
         B     VAL5                                                             
VAL4     NI    FIND,X'FE'          FIND=B'XXXXXXX0'                             
         MVC   TEMP(1),1(R4)       TEMP=B'XXXXXXXX'                             
         NC    TEMP(1),FIND        IS FLD FORMAT OK FOR REQ                     
         BNZ   VAL5                YES                                          
         MVI   FERN,6              ERROR FORMAT NOT AVAILABLE                   
         B     OERRMSG                                                          
         SPACE 2                                                                
VAL5     LA    RA,3(RA)            FIND NEXT REQ MAP ENTRY                      
         MVC   HALF,1(RA)                                                       
         LH    R5,HALF                                                          
         AR    R5,R3               R5=A(NEXT UNPROT FLD HDR)                    
         CLI   0(RA),126           CARD REQUEST FORMAT                          
         BNE   VAL7                                                             
         MVC   REQREC+28(78),8(R5) YES- MOVE SCREEN TO REQ REC                  
         B     UPDATE                                                           
         SPACE 2                                                                
VAL7     LA    R4,2(R4)            FIND NEXT REQ TBL FLD                        
         B     VAL1                                                             
         SPACE 2                                                                
VALX     CLI   0(RA),127           END OF REQ MAP TBL                           
         BE    UPDATE                                                           
         DC    H'0'                DUMP IF TBLS OUT OF STEP                     
         SPACE 2                                                                
UPDATE   MVI   OLAYNUM,02          PASS CONTROL TO OVERLAY                      
         GOTO1 AOLAY                                                            
         CLI   FERN,X'FF'          UPDATED/DISPLAYED OK                         
         BL    OERRMSG             NO ERROR                                     
         CLI   REQACTN,C'D'                                                     
         BNE   *+16                                                             
         CLI   STATUS,1                                                         
         BE    CONTROL                                                          
         B     UPDATE1                                                          
         XC    BVRHDR,BVRHDR                                                    
         CLI   REQACTN,C'A'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(15),=C'REQUEST AMENDED'                                   
         B     UPDATE1                                                          
         CLI   REQACTN,C'N'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(13),=C'REQUEST ADDED'                                     
         B     UPDATE1                                                          
UPDATE1  LA    R6,BVRNAMEH                                                      
         ST    R6,FADR                                                          
         MVI   STATUS,0                                                         
         MVC   PREQNUM,LREQNUM                                                  
         MVC   PREQACTN,LREQACTN                                                
         MVC   PREQNDX1(2),LREQNDX1                                             
         B     OHDR                                                             
         EJECT                                                                  
*        SET UP SCREEN HEADER MESSAGE TO CONTAIN THE ERROR                      
*                                                                               
OERRMSG  XC    BVRHDR,BVRHDR                                                    
         SR    R5,R5                                                            
         IC    R5,FERN                                                          
         L     R6,DATAMGR                                                       
         GOTO1 GETMSG,PLIST,((R5),BVRHDR),(12,DMCB),(0(R3),(R6))                
         SPACE 2                                                                
*        OUTPUT SCREEN HEADER MESSAGE , POSITION THE CURSOR , AND               
*        RETURN TO TERMINAL                                                     
*                                                                               
OHDR     OI    BVRHDRH+6,OI1T                                                   
         MVI   BVRHDRH+7,60                                                     
         L     R6,FADR                                                          
         OI    6(R6),OI1C                                                       
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*        THIS ROUTINE PASSES CONTROL TO THE OVERLAY NUMBER SPECIFIED BY         
*        OLAYNUM.IF THE OVERLAY IS NOT IN CORE IT IS READ FROM DISK AND         
*        ITS NUM AND ADDRESS ARE SAVED.                                         
*                                                                               
OLAY     NTR1  BASE=ABASE                                                       
         CLC   OLAYNUM,LOLAYNUM    IS OVERLAY IN CORE                           
         BE    OLAY1               YES                                          
         XC    CALLOVPL(8),CALLOVPL                                             
         MVC   CALLOVPL(1),OLAYNUM                                              
         ST    R3,CALLOVPL+4                                                    
         GOTO1 CALLOV,CALLOVPL                                                  
         CLI   CALLOVPL+4,X'FF'                                                 
         BNE   *+6                                                              
         DC    H'0'                DUMP IF CANT LOAD                            
         MVC   LOLAYNUM(4),CALLOVPL                                             
OLAY1    GOTO1 LOLAYNUM,PHASEPL,(R9)                                            
OLAYX    XIT1                                                                   
         EJECT                                                                  
*        THIS ROUTINE READS THE CPPFILE RECORD SPECIFIED BY KEY AND             
*        SETS FERN TO X'FF' IF OK , TO X'FE' IF NOT FOUND, OR TO X'00'          
*        IF A DISK ERROR IS ENCOUNTERED.                                        
*                                                                               
READFIL  NTR1  BASE=ABASE                                                       
         XC    NAME,NAME           CLEAR NAME                                   
         MVC   TEMP(L'KEY),KEY                                                  
         GOTO1 DATAMGR,DMCB,(1,DMREAD),FILNAME,TEMP,FILREC                      
         CLI   DMCB+8,0                                                         
         BE    RFIL2               FOUND OK                                     
         TM    DMCB+8,X'10'                                                     
         BO    RFIL1               NOT FOUND                                    
         MVI   FERN,0                                                           
         B     RFILX               DISK ERROR                                   
RFIL1    MVI   FERN,X'FE'                                                       
         B     RFILX                                                            
RFIL2    MVI   FERN,X'FF'                                                       
RFILX    XIT1                                                                   
         SPACE 2                                                                
DMREAD   DC    C'DMREAD'                                                        
FILNAME  DC    C'CPFILE'                                                        
FRSTEL   EQU   19                                                               
         EJECT                                                                  
*        INITIALISE FIND,FERN,FADR FOR FLD NUM N AT TWA HDR AT R1.              
*        MOVE FLD TO IFLD AND SET R4=A(HDR) & R5=L'DATA.                        
*        SET R6=A(NEXT TWA FIELD)                                               
*                                                                               
INITV    NTR1  BASE=ABASE                                                       
         XC    HALF,HALF           SAVE FLD NUM                                 
         MVC   HALF+1(1),ROUTSUB                                                
         MVI   IFLDH+1,X'FF'                                                    
         XC    IFLDH,IFLDH                                                      
         MVC   IFLD,=CL32' '                                                    
         XC    NAME,NAME                                                        
         LR    R4,R1                                                            
         SR    R5,R5                                                            
         IC    R5,5(R4)                                                         
         LTR   R5,R5               NULLS OR SPACES NOT INPUT                    
         BZ    INITV1                                                           
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),=CL32' '                                                 
         BNE   INITV2                                                           
*                                                                               
INITV1   SR    R5,R5               MISSING FLD                                  
         MVI   IFLDH+1,01                                                       
         B     INITVX                                                           
*                                                                               
INITV2   LA    R5,1(R5)            R5=TOTAL INPUT COUNT                         
         SR    R1,R1               R1=DELIMITER COUNT                           
         LA    R2,8(R4)            R2=A(FLD START)                              
         LR    R3,R2               R3=A(FLD NEXT CHR)                           
         CLI   ROUTSUB,0           INHIBIT SCAN FOR DELIMITERS                  
         BE    INITV7                                                           
*                                                                               
INITV3   CLI   0(R3),C','          SCAN FOR DELIMITER                           
         BE    INITV5                                                           
         CLI   0(R3),C'-'                                                       
         BE    INITV5                                                           
INITV4   LA    R3,1(R3)                                                         
         BCT   R5,INITV3                                                        
*                                                                               
INITV5   LA    R1,1(R1)            BUMP DELIMITER COUNT                         
         CH    R1,HALF                                                          
         BE    INITV6              FLD#N R2=START R3=END+1                      
         LTR   R5,R5                                                            
         BZ    INITV1                                                           
         LA    R2,1(R3)                                                         
         B     INITV4                                                           
*                                                                               
INITV6   LR    R5,R3                                                            
         SR    R5,R2               R5=FLD#N LENGTH                              
         BZ    INITV1                                                           
INITV7   STC   R5,IFLDH+5                                                       
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),0(R2)                                                    
         LA    R5,1(R5)                                                         
         OI    IFLDH,X'01'          SET FLD INPUT                               
         CLI   IFLDH+5,3                                                        
         BNE   INITVX                                                           
         CLC   IFLD(3),=C'ALL'                                                  
         BNE   INITVX                                                           
         OI    IFLDH,X'02'          SET FLD INPUT=C'ALL'                        
*                                                                               
INITVX   SR    R6,R6               SET R6=A(NEXT TWA FLD HDR)                   
         IC    R6,0(R4)                                                         
         AR    R6,R4                                                            
         CLI   ROUTSUB,1                                                        
         BH    *+14                                                             
         MVC   FIND,IFLDH                                                       
         ST    R4,FADR                                                          
         MVC   FERN,IFLDH+1                                                     
         TM    1(R6),X'20'         IS FLD PROTECTED                             
         BZ    INITVX1             NO                                           
         TM    REQFMT,X'20'        2UP FORMAT                                   
         BO    INITVX1             YES                                          
         MVC   HALF,2(R6)                                                       
         SR    R2,R2                                                            
         LH    R3,HALF                                                          
         D     R2,=F'80'                                                        
         CH    R2,=H'1'            DOES FLD START IN COL#2                      
         BE    INITVX1             YES IGNORE                                   
         CLI   FLDCMNT,X'81'       IS FLD STANDARD COMMENT                      
         BNE   INITVX1             NO IGNORE                                    
         SR    R2,R2                                                            
         IC    R2,0(R6)                                                         
         SH    R2,=H'9'                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         XC    8(0,R6),8(R6)       CLEAR & TRANSMIT COMMENT ENTRIES             
         OI    6(R6),X'80'                                                      
         B     *+6                                                              
INITVX1  SR    R6,R6                                                            
         XIT1  REGS=(R4,R6)                                                     
         EJECT                                                                  
*        ROUTINE TO RIGHT JUSTIFY A FLD OF LENGTH R5 AT HDR ADR R4 AND          
*        CHECK FOR NUMERIC GT 0 AND LE 255. RETURN IN TEMP                      
*        CL1   BINARY VALUE                                                     
*        CL3   RJ JUSTIFIED VALUE                                               
*                                                                               
RJN      NTR1  BASE=ABASE                                                       
         MVC   TEMP(4),=C'0000'                                                 
         MVI   FERN,X'FF'                                                       
         LR    R8,R5                                                            
         BCTR  R8,R0                                                            
         LA    R7,3                                                             
         SR    R7,R5                                                            
         LA    R7,TEMP+1(R7)                                                    
         EX    R8,*+8              RJ JUSTIFY AT TEMP+1(3)                      
         B     *+10                                                             
         MVC   0(0,R7),8(R4)                                                    
         MVC   TEMP+10(3),=C'0000' CHECK FOR NUMERIC                            
         MVZ   TEMP+10(3),TEMP+1                                                
         CLC   TEMP+10(3),=C'0000'                                              
         BE    *+12                                                             
         MVI   FERN,3                                                           
         B     RJNX                                                             
         PACK  DUB,TEMP+1(3)       CHECK GT 0 AND LE 255                        
         CVB   R7,DUB                                                           
         CH    R7,=H'0'                                                         
         BE    RJN1                                                             
         CH    R7,=H'255'                                                       
         BH    RJN1                                                             
         STC   R7,TEMP             RETURN BINARY VALUE                          
         B     RJNX                                                             
RJN1     MVI   FERN,02             SET ERROR CODE                               
RJNX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        THIS TABLE CONTAINS AN ENTRY FOR EACH REQUEST FIELD DEFINED            
*        EACH ENTRY HAS THE FORMAT :-                                           
*        CL1   FLD NUM (=0 END OF TBL)                                          
*        CL1   OVERLAY NUM OF PHASE CONTAINING VALIDATION CODE                  
*        CL1   ROUTINE NUM IN PHASE FOR FLD                                     
*        CL1   SUB FLD NUM - 1=FIRST - DENOTES MULTI FIELD POSITION             
FLDNUMT  DS    0CL4                                                             
*                                                                               
         DC    AL1(01,3,01,0)      RANGE                                        
         DC    AL1(02,3,02,0)      MARKET SEQUENCE                              
         DC    AL1(03,3,03,0)      MARKET                                       
         DC    AL1(04,3,04,0)      UNIVERSE                                     
         DC    AL1(05,3,05,0)      DATA TYPE LIST                               
         DC    AL1(06,3,06,1)      START DATE                                   
         DC    AL1(07,3,07,2)      END DATE                                     
         DC    AL1(08,3,08,0)      TIME ZONE                                    
         DC    AL1(09,3,09,0)      SPOT LENGTH                                  
         DC    AL1(10,3,10,0)      DAY PART                                     
         DC    AL1(11,3,11,0)      AFFILIATION                                  
         DC    AL1(12,3,12,0)      REPORT DEMO                                  
         DC    AL1(13,3,13,0)      BASE DATE                                    
         DC    AL1(14,3,14,0)      PROJECTION FORMULA                           
         DC    AL1(15,3,15,0)      RATING SERVICE FILTER                        
         DC    AL1(16,3,16,0)      PROGRAM TYPE                                 
         DC    AL1(17,3,17,0)      OPTION#1                                     
         DC    AL1(18,3,18,0)      OPTION#2                                     
         DC    AL1(19,3,19,0)      OPTION#3                                     
         DC    AL1(20,3,20,0)      OPTION#4                                     
         DC    AL1(21,3,21,0)      OPTION#5                                     
         DC    AL1(22,3,22,0)      OPTION#6                                     
         DC    AL1(23,3,04,0)      TARGET UNIVERSE                              
         DC    AL1(24,3,18,0)      DAYPART CONTROL                              
         DC    AL1(25,3,12,0)      DEMOGROUP                                    
         DC    AL1(26,3,23,0)      REQUEST PROGRAM                              
         DC    AL1(27,3,19,0)      MARKET SORT                                  
         DC    AL1(28,3,01,0)      RANGE(CLT/OFF)                               
*                                                                               
FLDNUMTX DC    XL1'00'                                                          
         EJECT                                                                  
*        THIS TABLE CONTAINS A VARIABLE LENGTH ENTRY FOR EACH REQUEST           
*        CL1   ENTRY LENGTH - ZERO=END-OF-TABLE                                 
*        CL1   REQUEST NUM                                                      
*        CL1   MEDIA BITS - B'0000000T'                                         
*        CLI   INPUT BITS - 01=NAME,02=MEDIA,04=DDS,08=CRD                      
*        CL22  REQUEST NAME                                                     
*        CL1   DEFAULT VALUES ROUTINE NUM                                       
*        CL1   FURTHER VALIDATION ROUTINE NUM                                   
*        0CLN  ENTRY FOR EACH SCREEN FOR REQUEST                                
*        CL1   ENTRY LENGTH                                                     
*        CL1   MEDIA BITS - B'0000000T'                                         
*        CL2   DEFN OF EACH FIELD - CL1 FIELD NUM - CL1 FIELD FORMAT            
*              FIELD NUM EQ 000 END-OF-FIELD LIST                               
*                        LE 127 DATA FIELD                                      
*                        GE 127 COMMENT - ODD=SAME LINE - EVEN=NEW LINE         
*              FIELD FMT EQ X'80' DDS ONLY FIELD FOR 1UP/2UP                    
*                           X'01' OPTIONAL FIELD                                
*                           X'7E' FMT BITS FIELD                                
*        XL1   ZERO FOR END OF ENTRY                                            
*        CL2   ALPHA REQUEST ID                                                 
         SPACE 2                                                                
REQTBL   DS    0CL1                                                             
         SPACE 2                                                                
RT00     DC    AL1(RT00X-*+3,00)   00 - UNKNOWN REQUEST                         
         DC    X'FF03',CL22'???-UNKNOWN'                                        
         DC    AL1(00,00)                                                       
         DC    AL1(RT00X-*+1),X'FF'                                             
         DC    X'017F'             RNGE=OPT/ANY                                 
         DC    X'027F'             MKTS=OPT/ANY                                 
         DC    X'037F'             MKTV=OPT/ANY                                 
         DC    X'047F8100'         UNIV=OPT/NNN                                 
         DC    X'0C7F8100'         RDEM=OPT/NNN                                 
         DC    X'057F'             LIST=OPT/ANY                                 
         DC    X'0608'             STRD=YYMM                                    
         DC    X'0708'             ENDD=YYMM                                    
         DC    X'0D09'             BASD=OPT/YYMM                                
         DC    X'0E7F'             PRJF=OPT/ANY                                 
*&&US*&& DC    X'0F7F'             RTGF=OPT/ANY                                 
*&&US*&& DC    X'087F'             ZONE=OPT/ANY                                 
         DC    X'097F'             SPTL=OPT/ANY                                 
         DC    X'0A7F'             DAYP=OPT/ANY                                 
*&&US*&& DC    X'0B7F'             AFFL=OPT/ANY                                 
         DC    X'107F'             P TYPE=ANY                                   
         DC    X'117F'             OPT1=OPT/ANY                                 
         DC    X'127F'             OPT2=OPT/ANY                                 
RT00X    DC    X'00',C'00'                                                      
         SPACE 2                                                                
RT20     DC    AL1(RT20X-*+3,20)   20 - CPP GUIDE                               
         DC    X'FF03',CL22'CPG-CPP GUIDE'                                      
         DC    AL1(00,01)                                                       
         DC    AL1(RT20X-*+1),X'FF'                                             
         DC    X'017D'             RNGE=OPT/A/B/C/O/G                           
         DC    X'021D'             MKTS=OPT/A/R/C                               
         DC    X'030F'             MKTV=OPT/ALL/NNNN/GROUP                      
         DC    X'04058100'         UNIV=OPTN/NNN                                
         DC    X'19058100'         DGRP=OPT/NNN                                 
         DC    X'0505'             LIST=OPT/N                                   
         DC    X'0608'             STRD=YYMM                                    
         DC    X'07088B00'         ENDD=YYMM                                    
         DC    X'0D09'             BASD=OPT/YYMM                                
         DC    X'0E05'             PRJF=OPT/X                                   
*&&US*&& DC    X'0F05'             RTGF=OPT/A/N                                 
*&&US*&& DC    X'0805'             ZONE=OPT/N                                   
         DC    X'0905'             SPTL=OPT/NNN                                 
         DC    X'0A05'             DAYP=OPT/XXX                                 
*&&US*&& DC    X'0B05'             AFFL=OPT/X                                   
         DC    X'1005'             P TYPE=OPT/ANY ALPHA                         
         DC    X'11058300'         OPT1=OPT/Y                                   
         DC    X'12058500'         OPT2=OPT/Y                                   
         DC    X'13058700'         OPT3=OPT/Y                                   
RT20X    DC    X'00',C'20'                                                      
         SPACE 2                                                                
RT22     DC    AL1(RT22X-*+3,22)   22 - TREND REPORT                            
         DC    X'FF03',CL22'TRN-TREND REPORT'                                   
         DC    AL1(00,00)                                                       
         DC    AL1(RT22X-*+1),X'FF'                                             
         DC    X'017D'             RNGE=OPT/A/B/C/O/G                           
         DC    X'021D'             MKTS=OPT/A/R/C                               
         DC    X'030F'             MKTV=OPT/ALL/NNNN/GROUP                      
         DC    X'04058100'         UNIV=OPT/NNN                                 
         DC    X'0C048100'         RDEM=OPT/NNN                                 
         DC    X'0505'             LIST=OPT/N                                   
         DC    X'0608'             STRD=YYMM                                    
         DC    X'07088B00'         ENDD=YYMM                                    
         DC    X'0D09'             BASD=OPT/YYMM                                
         DC    X'0E05'             PRJF=OPT/X                                   
*&&US*&& DC    X'0F05'             RTGF=OPT/A/N                                 
*&&US*&& DC    X'0805'             ZONE=OPT/N                                   
         DC    X'0905'             SPTL=OPT/NNN                                 
         DC    X'0A05'             DAYP=OPT/XXX                                 
*&&US*&& DC    X'0B05'             AFFL=OPT/X                                   
         DC    X'1005'             P TYPE=OPT/ANY ALPHA                         
         DC    X'11058300'         OPT1=OPT/Y                                   
         DC    X'12058500'         OPT2=OPT/Y                                   
RT22X    DC    X'00',C'22'                                                      
         SPACE 2                                                                
RT26     DC    AL1(RT26X-*+3,26)   26 - MARKET FOCUS                            
         DC    X'FF03',CL22'MKF-MARKET FOCUS'                                   
         DC    AL1(00,01)                                                       
         DC    AL1(RT26X-*+1),X'FF'                                             
         DC    X'0131'             RNGE=OPT/C/O                                 
         DC    X'021D'             MKTS=OPT/A/R/C                               
         DC    X'030F'             MKTV=OPT/ALL/NNNN/GROUP                      
         DC    X'17058100'         TUNIV=OPT/NNN                                
         DC    X'0C058100'         RDEM=NNN                                     
         DC    X'0608'             STRD=YYMM                                    
         DC    X'07088B00'         ENDD=YYMM                                    
         DC    X'0D09'             BASD=OPT/YYMM                                
         DC    X'0E05'             PRJF=OPT/X                                   
*&&US*&& DC    X'0F05'             RTGF=OPT/A/N                                 
*&&US*&& DC    X'0805'             ZONE=OPT/N                                   
         DC    X'0905'             SPTL=OPT/NNN                                 
*&&US*&& DC    X'0B05'             AFFL=OPT/X                                   
         DC    X'1005'             P TYPE=OPT/ANY ALPHA                         
         DC    X'11058300'         OPT1=OPT/Y                                   
         DC    X'18058900'         DCNL=OPT/1-9                                 
RT26X    DC    X'00',C'26'                                                      
         SPACE 2                                                                
RT28     DC    AL1(RT28X-*+3,28)   28 - MARKET SUMMARY                          
         DC    X'FF03',CL22'MKS-MARKET SUMMARY'                                 
         DC    AL1(00,01)                                                       
         DC    AL1(RT28X-*+1),X'FF'                                             
         DC    X'017D'             RNGE=OPT/A/B/C/O/G                           
         DC    X'021D'             MKTS=OPT/A/R/C                               
         DC    X'030B'             MKTV=OPT/ALL/GROUP                           
         DC    X'04058100'         UNIV=OPT/NNN                                 
         DC    X'0C058100'         RDEM=OPT/NNN                                 
         DC    X'0608'             STRD=YYMM                                    
         DC    X'07088B00'         ENDD=YYMM                                    
         DC    X'0D09'             BASD=OPT/YYMM                                
         DC    X'0E05'             PRJF=OPT/X                                   
*&&US*&& DC    X'0F05'             RTGF=OPT/A/N                                 
*&&US*&& DC    X'0805'             ZONE=OPT/N                                   
         DC    X'0905'             SPTL=OPT/NNN                                 
*&&US*&& DC    X'0B05'             AFFL=OPT/X                                   
         DC    X'1005'             P TYPE=OPT/ANY ALPHA                         
*&&US*&& DC    X'11058300'         OPT1=OPT/Y                                   
         DC    X'18058900'         DCNL=OPT/1-9                                 
RT28X    DC    X'00',C'28'                                                      
         SPACE 2                                                                
RT30     DC    AL1(RT30X-*+3,30)   30 - ONE SPOT PER MARKET                     
         DC    X'FF03',CL22'ONE-ONE SPOT/MKT'                                   
         DC    AL1(01,01)                                                       
         DC    AL1(RT30X-*+1),X'FF'                                             
         DC    X'017D'             RNGE=OPT/A/B/C/O/G                           
         DC    X'04058100'         UNIV=OPT/NNN                                 
         DC    X'0C058100'         RDEM=OPT/NNN                                 
         DC    X'0608'             STRD=YYMM                                    
         DC    X'07088B00'         ENDD=YYMM                                    
         DC    X'0D09'             BASD=OPT/YYMM                                
         DC    X'0E05'             PRJF=OPT/X                                   
*&&US*&& DC    X'0F05'             RTGF=OPT/A/N                                 
         DC    X'0905'             SPTL=OPT/NNN                                 
*&&US*&& DC    X'0B05'             AFFL=OPT/X                                   
         DC    X'1005'             P TYPE=OPT/ANY ALPHA                         
*&&US*&& DC    X'11058300'         OPT1=OPT/Y                                   
         DC    X'18058900'         DCNL=OPT/1-9                                 
RT30X    DC    X'00',C'30'                                                      
         SPACE 2                                                                
RT36     DC    AL1(RT36X-*+3,36)   36 - CPP/CPM SUMMARY                         
         DC    X'FF03',CL22'MK1-CPP/M SUMMARY'                                  
         DC    AL1(00,01)                                                       
         DC    AL1(RT36X-*+1),X'FF'                                             
         DC    X'1C7D'             RNGE=OPT/A/B/C/O/G                           
         DC    X'021D'             MKTS=OPT/A/R/C                               
         DC    X'030F'             MKTV=OPT/ALL/NNNN/GROUP                      
         DC    X'04058100'         UNIV=OPT/NNN                                 
         DC    X'0C058100'         RDEM=OPT/NNN                                 
         DC    X'0608'             STRD=YYMM                                    
         DC    X'07088B00'         ENDD=YYMM                                    
*&&US*&& DC    X'0F05'             RTGF=OPT/A/N                                 
         DC    X'10058F00'         P TYPE=OPT A                                 
*&&US*&& DC    X'1B05'         MARKET ORDER                                     
RT36X    DC    X'00',C'36'                                                      
         SPACE 2                                                                
RT38     DC    AL1(RT38X-*+3,38)   38 - TREND REPORT                            
         DC    X'FF03',CL22'NTR-TREND REPORT'                                   
         DC    AL1(00,00)                                                       
         DC    AL1(RT38X-*+1),X'FF'                                             
         DC    X'1C7D'             RNGE=OPT/A/B/C/O/G                           
         DC    X'021D'             MKTS=OPT/A/R/C                               
         DC    X'030F'             MKTV=OPT/ALL/NNNN/GROUP                      
         DC    X'04058100'         UNIV=OPT/NNN                                 
         DC    X'0C048100'         RDEM=OPT/NNN                                 
         DC    X'0505'             LIST=OPT/N                                   
         DC    X'0608'             STRD=YYMM                                    
         DC    X'07088B00'         ENDD=YYMM                                    
*&&US*&& DC    X'0F05'             RTGF=OPT/A/N                                 
         DC    X'10058F00'         P TYPE=OPT A                                 
RT38X    DC    X'00',C'38'                                                      
         SPACE 2                                                                
RT4T     DC    AL1(RT4TX-*+3,41)   41 - CUSTOMIZED RPT - TAPE EXTRACT           
         DC    X'FF03',CL22'CUT-CUSTOMIZED TAPE'                                
         DC    AL1(00,01)                                                       
         DC    AL1(RT4TX-*+1),X'FF'                                             
         DC    X'1A04'             REQ PGM=XXXX                                 
         DC    X'011D'             RNGE=OPT/A/B/C                               
         DC    X'021D'             MKTS=OPT/A/R/C                               
         DC    X'030F'             MKTV=OPT/ALL/NNNN/GROUP                      
         DC    X'04058100'         UNIV=OPTN/NNN                                
         DC    X'19058100'         DGRP=OPT/NNN                                 
         DC    X'0505'             LIST=OPT/N                                   
         DC    X'0608'             STRD=YYMM                                    
         DC    X'07088B00'         ENDD=YYMM                                    
         DC    X'0D098B00'         BASD=OPT/YYMM                                
*&&US*&& DC    X'0F04'             RTGF=OPT/A/N                                 
         DC    X'0E05'             PRJF=OPT/X                                   
         DC    X'0905'             SPTL=OPT/NNN                                 
         DC    X'0A05'             DAYP=OPT/XXX                                 
*&&US*&& DC    X'0B05'             AFFL=OPT/X                                   
         DC    X'1005'             P TYPE=OPT/ANY ALPHA                         
         DC    X'117F'             OPT1=OPT/ANY                                 
         DC    X'127F'             OPT2=OPT/ANY                                 
         DC    X'13058700'         OPT3=OPT/Y                                   
RT4TX    DC    X'00',C'4T'                                                      
         SPACE 2                                                                
RT40     DC    AL1(RT40X-*+3,40)   40 - CUSTOMIZED RPT                          
         DC    X'FF03',CL22'CUS-CUSTOMIZED REPORT'                              
         DC    AL1(00,01)                                                       
         DC    AL1(RT40X-*+1),X'FF'                                             
         DC    X'1A04'             REQ PGM=XXXX                                 
         DC    X'011D'             RNGE=OPT/A/B/C                               
         DC    X'021D'             MKTS=OPT/A/R/C                               
         DC    X'030F'             MKTV=OPT/ALL/NNNN/GROUP                      
         DC    X'04058100'         UNIV=OPTN/NNN                                
         DC    X'19058100'         DGRP=OPT/NNN                                 
         DC    X'0505'             LIST=OPT/N                                   
         DC    X'0608'             STRD=YYMM                                    
         DC    X'07088B00'         ENDD=YYMM                                    
         DC    X'0D098B00'         BASD=OPT/YYMM                                
*&&US*&& DC    X'0F04'             RTGF=OPT/A/N                                 
         DC    X'0E05'             PRJF=OPT/X                                   
         DC    X'0905'             SPTL=OPT/NNN                                 
         DC    X'0A05'             DAYP=OPT/XXX                                 
*&&US*&& DC    X'0B05'             AFFL=OPT/X                                   
         DC    X'1005'             P TYPE=OPT/ANY ALPHA                         
         DC    X'117F'             OPT1=OPT/ANY                                 
         DC    X'127F'             OPT2=OPT/ANY                                 
         DC    X'13058700'         OPT3=OPT/Y                                   
RT40X    DC    X'00',C'40'                                                      
         SPACE 2                                                                
RT42     DC    AL1(RT42X-*+3,42)   42 - CLIENT INDEX                            
         DC    X'FF03',CL22'CLI-CLIENT INDEX'                                   
         DC    AL1(00,01)                                                       
         DC    AL1(RT42X-*+1),X'FF'                                             
         DC    X'1C30'             RNGE=OPT/C/O                                 
         DC    X'021D'             MKTS=OPT/A/R/C                               
         DC    X'030F'             MKTV=OPT/ALL/NNNN/GROUP                      
         DC    X'04058100'         UNIV=OPT/NNN                                 
         DC    X'0C058100'         RDEM=OPT/NNN                                 
         DC    X'0608'             STRD=YYMM                                    
         DC    X'07088B00'         ENDD=YYMM                                    
*&&US*&& DC    X'0F05'             RTGF=OPT/A/N                                 
         DC    X'10058F00'         P TYPE=OPT A                                 
*&&US*&& DC    X'1B05'         MARKET ORDER                                     
RT42X    DC    X'00',C'42'                                                      
         SPACE 2                                                                
REQTBLX  DC    X'00'                                                            
         EJECT                                                                  
*CPREQSAVE                                                                      
       ++INCLUDE CPREQSAVE                                                      
*CPREQTEMP                                                                      
       ++INCLUDE CPREQTEMP                                                      
*CPREQFF                                                                        
       ++INCLUDE CPREQFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029CPREQ00   09/01/00'                                      
         END                                                                    
