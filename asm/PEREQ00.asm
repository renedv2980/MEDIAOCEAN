*          DATA SET PEREQ00    AT LEVEL 002 AS OF 08/22/00                      
*PHASE TE0400A                                                                  
         TITLE 'PEREQ00 - REQUEST - ROOT CONTROLLER'                            
TE0400   CSECT                                                                  
*        REWRITTEN FROM MEDIA REQ PROG                                          
         PRINT NOGEN                                                            
         NMOD1 REQTEMPX-REQTEMP,**RQ00**,RR=R4                                  
         LR    R9,RC                                                            
         USING REQTEMP,R9          R9=A(W/S)                                    
         ST    R9,ATEMP                                                         
         ST    R4,RELO                                                          
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
         MVC   AID,6(R2)           STORE INPUT AID PFKEY                        
*                                                                               
         MVI   DDS,0                                                            
         CLI   1(R3),C'*'          SET DDS TERMINAL                             
         BNE   *+8                                                              
         MVI   DDS,1                                                            
         MVC   AGYB,0(R1)          STORE AGY BINARY                             
         MVC   AGY,14(R3)          STORE AGY ALPHA                              
         MVC   USRID,10(R3)        STORE UESR ID NUMBER                         
         MVI   USRIDF,0                                                         
         L     R4,16(R1)                                                        
         ST    R4,ACOMFACS                                                      
         MVC   FACLIST,0(R4)       STORE COMMON FACILITY LIST                   
         GOTO1 GETFACT,PLIST,0                                                  
         L     R1,0(R1)                                                         
         MVC   TODAY(13),4(R1)                                                  
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
         LA    R5,BVRTABH          POSN CURSOR                                  
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
         LA    R6,BVRTABH                                                       
         C     R6,AFIRSTF          WAS A REQ DEFN FLD INPUT                     
         BL    *+12                NO                                           
         MVI   STATUS,0            YES BACK TO STATUS 0                         
         B     CONTROL                                                          
         CLI   REQACTN,C'D'                                                     
         BE    UPDATE                                                           
         B     VALIDATE                                                         
         SPACE 2                                                                
STATUS3  B     UPDATE              PROCESS DISP/CARD                            
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
VAL3     NC    1(2,RA),1(RA)       IS THIS FIELD PRESENT ?                      
         BZ    VAL5                NO - IGNORE IT                               
VAL3A    MVC   OLAYNUM,1(R6)       SET OVERLAY REQD                             
         MVC   ROUTNUM,2(R6)       SET VALIDATION ROUT REQD                     
         MVC   ROUTSUB,3(R6)       SET SUB FLD NUM                              
         MVC   FLDCMNT,2(R4)       SET INPUT FLD COMMENT NUMBER                 
*                                                                               
         ST    R4,REQTADR          SET REQUEST TABLE ENTRY ADR                  
         ST    R5,FLDHADR          SET INPUT FLD HDR ADR                        
*                                                                               
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
         MVI   FERN,46             ERROR FORMAT NOT AVAILABLE                   
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
         GOTO1 GETMSG,PLIST,((R5),BVRHDR),(06,DMCB),(0(R3),(R6))                
         SPACE 2                                                                
*        OUTPUT SCREEN HEADER MESSAGE , POSITION THE CURSOR , AND               
*        RETURN TO TERMINAL                                                     
*                                                                               
OHDR     OI    BVRHDRH+6,X'80'                                                  
         MVI   BVRHDRH+7,60                                                     
         L     R6,FADR                                                          
         OI    6(R6),X'40'                                                      
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
*        THIS ROUTINE READS THE MEDFILE RECORD SPECIFIED BY KEY AND             
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
RFILX    CLI   FERN,X'FE'          LOW=DISKERR,EQL=NOTFOUND,HIGH=OK             
         XIT1                                                                   
         SPACE 2                                                                
DMREAD   DC    CL8'DMREAD'                                                      
FILNAME  DC    CL8'PERFIL'                                                      
FRSTEL   EQU   44                                                               
         EJECT                                                                  
*        INITIALISE FIND,FERN,FADR FOR FLD NUM N AT TWA HDR AT R1.              
*        MOVE FLD TO IFLD AND SET R4=A(HDR) & R5=L'DATA.                        
*        SET R6=A(NEXT TWA FIELD)                                               
*                                                                               
INITV    NTR1  BASE=ABASE                                                       
         XC    HALF,HALF           SAVE FLD NUM                                 
         MVC   HALF+1(1),ROUTSUB                                                
         MVI   FIND,0                                                           
         MVI   FERN,X'FF'                                                       
         ST    R1,FADR                                                          
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
         MVI   FERN,01                                                          
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
         OI    FIND,X'01'          SET FLD INPUT                                
         CLI   IFLDH+5,3                                                        
         BNE   INITVX                                                           
         CLC   IFLD(3),=C'ALL'                                                  
         BNE   INITVX                                                           
******** OI    FIND,X'02'          SET FLD INPUT=C'ALL'                         
*                                                                               
INITVX   SR    R6,R6               SET R6=A(NEXT TWA FLD HDR)                   
         IC    R6,0(R4)                                                         
         AR    R6,R4                                                            
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
INITVX1  SR    R6,R6               SET R6=ZERO IF NO COMMENT ENTRY              
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
         MVI   FERN,43                                                          
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
FLDNUMT  DS    0XL4                                                             
*                                                                               
         DC    AL1(011,3,14,0)     X'0B' DATE                                   
         DC    AL1(012,3,12,0)     X'0C' TOWN OR POSTCODE FILTER                
         DC    AL1(013,3,13,0)     X'0D' TOWN OR POSTCODE TYPE                  
         DC    AL1(014,3,14,0)     X'0E' START DATE                             
         DC    AL1(015,3,15,0)     X'0F' END DATE                               
         DC    AL1(016,3,16,0)     X'10' DATE TYPE                              
         DC    AL1(017,3,17,0)     X'11' AGE RANGE                              
         DC    AL1(018,3,18,0)     X'12' SEX                                    
         DC    AL1(019,3,19,0)     X'13' COUNTRY                                
         DC    AL1(020,3,20,0)     X'14' STATUS                                 
*                                                                               
         DC    AL1(033,3,33,0)     X'21' OPTION 1                               
         DC    AL1(034,3,34,0)     X'22' OPTION 2                               
         DC    AL1(035,3,35,0)     X'23' OPTION 3                               
         DC    AL1(036,3,36,0)     X'24' OPTION 4                               
         DC    AL1(037,3,37,0)     X'25' OPTION 5                               
         DC    AL1(038,3,38,0)     X'26' OPTION 6                               
         DC    AL1(039,3,39,0)     X'27' OPTION 7                               
*                                                                               
         DC    AL1(049,3,49,0)     X'31' SPECIAL 1                              
         DC    AL1(050,3,50,0)     X'32' SPECIAL 2                              
         DC    AL1(051,3,51,0)     X'33' SPECIAL 3                              
         DC    AL1(052,3,52,0)     X'34' SPECIAL 4                              
*                                                                               
FLDNUMTX DC    XL1'00'                                                          
         EJECT                                                                  
*        THIS TABLE CONTAINS A VARIABLE LENGTH ENTRY FOR EACH REQUEST           
*        CL1   ENTRY LEN  - ZERO=END-OF-TABLE                                   
*        CL1   REQUEST NUM                                                      
*        CL1   N/D                                                              
*        CLI   INPUT BITS - 01=NAME,04=DDS,08=CRD                               
*        CL22  REQUEST NAME                                                     
*        CL1   DEFAULT VALUES ROUTINE NUM                                       
*        CL1   FURTHER VALIDATION ROUTINE NUM                                   
*        0CLN  ENTRY FOR EACH SCREEN FOR REQUEST                                
*        CL1   ENTRY LENGTH                                                     
*        CL1   N/D                                                              
*        CL2   DEFN OF EACH FIELD - CL1 FIELD NUM - CL1 FIELD FORMAT            
*              FIELD NUM EQ 000 END-OF-FIELD LIST                               
*                        LE 127 DATA FIELD                                      
*                        GE 127 COMMENT - ODD=SAME LINE - EVEN=NEW LINE         
*              FIELD FMT EQ X'80' DDS ONLY FIELD FOR 1UP/2UP                    
*                           X'01' OPTIONAL FIELD                                
*                           X'7E' FMT BITS FIELD                                
*        XL1   ZERO FOR END OF ENTRY                                            
*        XL1   REQUEST GROUP                                                    
*        XL1   SPARE                                                            
*        CL2   ALPHA REQUEST ID                                                 
         SPACE 2                                                                
REQTBL   DS    0CL1                                                             
         SPACE 2                                                                
RT00     DC    AL1(RT00X-*+5,00)   00-UNKNOWN REQUEST                           
         DC    X'0001',CL22'???-UNKNOWN'                                        
         DC    AL1(00,00)                                                       
         DC    AL1(RT00X-*+1),X'00'                                             
         DC    X'0E0D'            *STRD=YYMMDD/YYMM                             
         DC    X'0F0D'            *ENDD=YYMMDD/YYMM                             
         DC    X'10058B00'        *TYPD=X                                       
         DC    X'1105'            *AGES=NN-NN                                   
         DC    X'1205'            *SEXC=M/F                                     
         DC    X'1305'            *CNTY=XX..                                    
         DC    X'0C058300'        *T/P=XX..                                     
         DC    X'0D058500'        *T/P=T/P                                      
         DC    X'14059300'        *STAT=A/L/B                                   
         DC    X'31059500'        *SPC1=Y/N                                     
         DC    X'32059500'        *SPC2=Y/N                                     
         DC    X'33059500'        *SPC3=Y/N                                     
         DC    X'34059500'        *SPC4=Y/N                                     
         DC    X'2105'            *OPT1=X                                       
         DC    X'2205'            *OPT2=X                                       
         DC    X'2305'            *OPT3=X                                       
         DC    X'2405'            *OPT4=X                                       
         DC    X'2505'            *OPT5=X (6 AND 7 DONT FIT)                    
RT00X    DC    X'00',X'00',X'00',C'00'                                          
         SPACE 2                                                                
RT10     DC    AL1(RT10X-*+5,10)   10-FILE LISTING                              
         DC    X'0001',CL22'LST-FILE LISTING'                                   
         DC    AL1(00,00)                                                       
         DC    AL1(RT10X-*+1),X'00'                                             
         DC    X'0E0D'            *STRD=YYMMDD/YYMM                             
         DC    X'0F0D'            *ENDD=YYMMDD/YYMM                             
         DC    X'10058B00'        *TYPD=X                                       
         DC    X'1105'            *AGES=NN-NN                                   
         DC    X'1205'            *SEXC=M/F                                     
         DC    X'1305'            *CNTY=XX..                                    
         DC    X'0C058300'        *T/P=XX..                                     
         DC    X'0D058500'        *T/P=T/P                                      
         DC    X'14059300'        *STAT=A/L/B                                   
         DC    X'31059500'        *SPC1=Y/N                                     
         DC    X'32059500'        *SPC2=Y/N                                     
         DC    X'33059500'        *SPC3=Y/N                                     
         DC    X'34059500'        *SPC4=Y/N                                     
         DC    X'2105C100'        *OPT1=123456                                  
         DC    X'2205C300'        *OPT2=123456                                  
         DC    X'2305C500'        *OPT3=123456                                  
         DC    X'2405C700'        *OPT4=Y                                       
RT10X    DC    X'00',X'00',X'00',C'10'                                          
         SPACE 2                                                                
RT20     DC    AL1(RT20X-*+5,20)   20-STICKY LABELS                             
         DC    X'0001',CL22'STK-STICK LABELS'                                   
         DC    AL1(00,00)                                                       
         DC    AL1(RT20X-*+1),X'00'                                             
         DC    X'0E0D'            *STRD=YYMMDD/YYMM                             
         DC    X'0F0D'            *ENDD=YYMMDD/YYMM                             
         DC    X'10058B00'        *TYPD=X                                       
         DC    X'1105'            *AGES=NN-NN                                   
         DC    X'1205'            *SEXC=M/F                                     
         DC    X'1305'            *CNTY=XX..                                    
         DC    X'0C058300'        *T/P=XX..                                     
         DC    X'0D058500'        *T/P=T/P                                      
         DC    X'14059300'        *STAT=A/L/B                                   
         DC    X'31059500'        *SPC1=Y/N                                     
         DC    X'32059500'        *SPC2=Y/N                                     
         DC    X'33059500'        *SPC3=Y/N                                     
         DC    X'34059500'        *SPC4=Y/N                                     
         DC    X'2105E100'        *OPT1=RC                                      
         DC    X'2205E300'        *OPT2=C                                       
         DC    X'2305E500'        *OPT3=T                                       
         DC    X'2405E700'        *OPT4=PN                                      
RT20X    DC    X'00',X'00',X'00',C'20'                                          
         SPACE 2                                                                
RT30     DC    AL1(RT30X-*+5,30)   30-FILE STATISTICS                           
         DC    X'0001',CL22'STA-FILE STATISTICS'                                
         DC    AL1(00,00)                                                       
         DC    AL1(RT30X-*+1),X'00'                                             
         DC    X'0E0D'            *STRD=YYMMDD/YYMM                             
         DC    X'0F0D'            *ENDD=YYMMDD/YYMM                             
         DC    X'10058B00'        *TYPD=X                                       
         DC    X'1105'            *AGES=NN-NN                                   
         DC    X'1205'            *SEXC=M/F                                     
         DC    X'1305'            *CNTY=XX..                                    
         DC    X'0C058300'        *T/P=XX..                                     
         DC    X'0D058500'        *T/P=T/P                                      
         DC    X'14059300'        *STAT=A/L/B                                   
         DC    X'31059500'        *SPC1=Y/N                                     
         DC    X'32059500'        *SPC2=Y/N                                     
         DC    X'33059500'        *SPC3=Y/N                                     
         DC    X'34059500'        *SPC4=Y/N                                     
         DC    X'2104E900'        *OPT1=12                                      
         DC    X'2204EB00'        *OPT2=A-G                                     
RT30X    DC    X'00',X'00',X'00',C'30'                                          
         SPACE 2                                                                
RT40     DC    AL1(RT40X-*+5,40)   40-FEE SUMMARY                               
         DC    X'0001',CL22'FSM-FEE SUMMARY'                                    
         DC    AL1(01,00)        *SET STATUS=BOTH                               
         DC    AL1(RT40X-*+1),X'00'                                             
         DC    X'0E0C'            *STRD=YYMMDD/YYMM                             
         DC    X'0F0C'            *ENDD=YYMMDD/YYMM                             
         DC    X'2105BF00'        *OPT1=X                                       
RT40X    DC    X'00',X'00',X'00',C'40'                                          
         SPACE 2                                                                
RT50     DC    AL1(RT50X-*+5,50)   50-ACTIVITY SUMMARY                          
         DC    X'0001',CL22'ASM-ACTIVITY SUMMARY'                               
         DC    AL1(01,00)        *SET STATUS=BOTH                               
         DC    AL1(RT50X-*+1),X'00'                                             
         DC    X'0B08'            *DATE=YYMM                                    
RT50X    DC    X'00',X'00',X'00',C'50'                                          
REQTBLX  DC    X'00'                                                            
         EJECT                                                                  
*PEREQSAVE                                                                      
       ++INCLUDE PEREQSAVE                                                      
         EJECT                                                                  
         ORG   REQSAVE+64                                                       
*PEREQFFD                                                                       
       ++INCLUDE PEREQFFD                                                       
         EJECT                                                                  
*PEREQTEMP                                                                      
       ++INCLUDE PEREQTEMP                                                      
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PEREQ00   08/22/00'                                      
         END                                                                    
