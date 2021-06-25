*          DATA SET PPPUB17    AT LEVEL 011 AS OF 01/29/99                      
*PHASE T40617A,+0,NOAUTO                                                        
*INCLUDE PSIZEVAL                                                               
         TITLE 'CHANGE LOG'                                                     
*                                                                               
***********************************************************************         
*                                                                               
*   KWAN  1/99   ERROR ON REPEATED NON-BLEED AD SIZE CODE (3 CHARS)             
*                                                                               
*   SMYE  9/98   CHANGE TO NOT REQUIRE TRIM ELEMENT AND DO NOT                  
*                ALLOW ADD WITH NO ELEMENTS (I.E. RECLEN OF 33)                 
*                                                                               
*   BPLA  9/98   PUBIO INCREASED TO 4000  - PUBIO CLEAR FIXED                   
*                IF DELETED RECORD IS READ-DON'T SET OF LTLIND                  
*                DELETE BOTH 10 AND 20 ELEMENTS AT EDIT00                       
*                ONLY MARK POINTER DELETED IF NO ELEMENTS REMAIN                
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'T40617  PUBFILE MAINT: NON-BLEED AD SIZING'                     
         PRINT NOGEN                                                            
*                                                                               
T40617   CSECT                                                                  
         NMOD1 0,T40617,RR=R9                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         LA    R8,T40617+4095      R8 = SECOND BASE REGISTER                    
         LA    R8,1(R8)                                                         
         USING T40617+4096,R8                                                   
*                                                                               
         USING T406FFD,RA                                                       
*                                                                               
         LA    R9,PUBIO                                                         
         USING PUBZREC,R9          ** PRINTPAK PUB AD SIZING RECORD **          
*                                                                               
         MVI   ADDCHSW,0                                                        
         MVI   LTLIND,0                                                         
*                                                                               
STARTIO  DS    0H                                                               
*                                                                               
         XCEF  PUBIO,4000          CLEAR PUB RECORD AREA                        
*                                                                               
         XC    KEY,KEY                                                          
         XC    PUBADDR,PUBADDR                                                  
*                                                                               
         MVC   PUBZKMED,BMED       MEDIA                                        
         MVC   PUBZKPUB(6),BPUB    PUB                                          
         MVC   PUBZKAGY,AGYALPHA   AGENCY                                       
         MVI   PUBZKCOD,X'83'      RECORD CODE (PUB AD SIZING REC)              
         MVC   KEY(25),PUBZKEY                                                  
*                                                                               
         MVC   SVINBTS,DMINBTS     SAVE DMINBTS                                 
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         BAS   RE,HIGHPUB                                                       
         MVC   DMINBTS,SVINBTS     RESTORE DMINBTS                              
*                                                                               
         CLC   KEY(25),KEYSAVE     PUB AD SIZE RECORD FOUND ?                   
         BE    STARTGET            YES - GET THE RECORD                         
         CLI   BACT,1              NO - IS IT ADD ?                             
         BE    CLISCRN             YES                                          
         B     CKIND               NO                                           
*                                                                               
STARTGET BAS   RE,GETPUB                                                        
         MVC   PUBADDR,KEY+27      SAVE ADDRESS OF FOUND RECORD                 
*                                                                               
         CLI   KEY+25,X'FF'        WAS RECORD DELETED?                          
         BE    CKIND               DON'T SET ON LTLIND                          
*                                                                               
         OI    LTLIND,X'10'        IF REC EXISTS, TRIM ELEMENT EXISTS           
*                                                                               
CKIND    DS    0H                                                               
         CLI   BACT,1              ADD                                          
         BNE   CLISCRN                                                          
         TM    LTLIND,X'10'                                                     
         BZ    CLISCRN                                                          
         CLI   KEY+25,X'FF'        FLAGGED FOR DELETION ?                       
         BE    CLISCRN             YES - OK TO EDIT                             
CKINDERR LA    R3,COMBERR                                                       
         LA    R2,PBLACTH                                                       
         B     ERROR                                                            
*                                                                               
CLISCRN  CLI   BYTE2,1             FIRST TIME THROUGH, BYTE2 IS X'01'           
         BE    FORMATP                                                          
*                                  CLIENT SCREEN IN TWA SO EDIT IT              
*                                  UNLESS ACTION=DISPLAY                        
         CLI   BACT,2                                                           
         BH    FORMATP                                                          
*                                                                               
***********************************************************************         
*                                                                               
EDIT00   DS    0H                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CHELLO-COMFACSD)(RF)                                         
         GOTO1 (RF),DMCB,(C'D',=C'PUBFILE'),(X'10',PUBZREC),0,0                 
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 (RF),DMCB,(C'D',=C'PUBFILE'),(X'20',PUBZREC),0,0                 
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   RECLEN,PUBZREC+25                                                
         ZICM  R3,RECLEN,2                                                      
         BCTR  R3,0                                                             
         STH   R3,RECLEN                                                        
*                                                                               
         LA    R2,ADSTRWDH         FIRST SCR FIELD                              
         LA    R1,ADSDPLSH         LAST SCR FIELD                               
EDIT10   CR    R2,R1               PASS LAST SCR FIELD YET?                     
         BH    EDIT15              YES                                          
         CLI   5(R2),0                                                          
         BNE   EDIT20              HAS INPUT, GO VALIDATE IT                    
         BAS   RE,NEXTFLD                                                       
         B     EDIT10                                                           
EDIT15   MVI   ESWITCH,0           NO INPUT AT ALL                              
         B     EDITXX                                                           
*                                                                               
EDIT20   DS    0H                                                               
         CLI   ADSTRWDH+5,0        TRIM WIDTH INPUT ?                           
         BNE   EDIT22              YES                                          
         CLI   ADSTRDPH+5,0        TRIM DEPTH INPUT ?                           
         BE    EDIT25              NO - BOTH ZERO                               
         LA    R2,ADSTRWDH         TRIM WIDTH                                   
         LA    R3,1                INPUT MISSING MSG                            
         B     ERROR                                                            
EDIT22   XC    ELEAREA,ELEAREA                                                  
         LA    R7,ELEAREA                                                       
         USING PPPUBTD,R7          TRIM ELEM DSECT                              
         MVI   PPPUBTEL,PPPUBTEQ   TRIM ELEMENT CODE                            
         MVI   PPPUBTLN,PPPUBTLNQ  TRIM ELEMENT LENGTH                          
         MVI   PPPUBTUI,C'I'       TRIM UNIT INDICATOR (I=INCH)                 
*                                                                               
         LA    R2,ADSTRWDH         TRIM WIDTH                                   
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         LA    R3,1                INPUT MISSING MSG                            
         B     ERROR                                                            
         BAS   RE,CKWDDP           CHECK TRIM WIDTH                             
         BE    *+12                                                             
         LA    R3,2                                                             
         B     ERROR                                                            
         BAS   RE,NUMBCONV         CONVERT TRIM WIDTH TO BINARY                 
         BE    *+12                                                             
         LA    R3,2                                                             
         B     ERROR                                                            
         MVC   PPPUBTWU,WKINT      TRIM WIDTH (BINARY)                          
         MVC   PPPUBTWN,WKNUMTOR+3                                              
         MVC   PPPUBTWD,WKDENTOR+3                                              
         OI    ESWITCH,1           SOMETHING HAS BEEN ENTERED                   
*                                                                               
         LA    R2,ADSTRDPH         TRIM DEPTH                                   
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         LA    R3,1                INPUT MISSING MSG                            
         B     ERROR                                                            
         BAS   RE,CKWDDP           CHECK TRIM DEPTH                             
         BE    *+12                                                             
         LA    R3,2                                                             
         B     ERROR                                                            
         BAS   RE,NUMBCONV         CONVERT TRIM DEPTH TO BINARY                 
         BE    *+12                                                             
         LA    R3,2                                                             
         B     ERROR                                                            
         MVC   PPPUBTDU,WKINT      TRIM DEPTH (BINARY)                          
         MVC   PPPUBTDN,WKNUMTOR+3                                              
         MVC   PPPUBTDD,WKDENTOR+3                                              
         OI    ESWITCH,1           SOMETHING HAS BEEN ENTERED                   
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CHELLO-COMFACSD)(RF)                                         
         GOTO1 (RF),DMCB,(C'P',=C'PUBFILE'),PUBZREC,ELEAREA,0                   
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   RECLEN,PUBZREC+25                                                
         ZICM  R3,RECLEN,2                                                      
         BCTR  R3,0                                                             
         STH   R3,RECLEN                                                        
         DROP  R7                                                               
*                                                                               
EDIT25   LA    R7,ELEAREA                                                       
         USING PPPUBAD,R7          NON-BLEED AD SIZE ELEM DSECT                 
*                                                                               
         XC    CODELIST,CODELIST   LIST OF CODES FOR NO DUPLICATES              
         LA    R2,ADSSPFSH         FIRST SPACE FIELD                            
         LA    R0,20               TWENTY SETS TO BE VALIDATED                  
EDIT30   XC    ELEAREA,ELEAREA                                                  
*                                                                               
         MVI   WKFLAG,0                                                         
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         OI    WKFLAG,NOSPACE                                                   
         B     EDIT54                                                           
*                                                                               
         LA    R3,2                                                             
         GOTO1 =V(PSIZEVAL),DMCB,(0,8(R2)),WORK,RR=RELO                         
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                                                               
         LA    RE,CODELIST                                                      
EDIT35   CLC   0(3,RE),WORK        SEE IF CODE ALREADY IN LIST                  
         BE    ERROR               IS IN LIST, DUPLICATE IS NOT ALLOWED         
         CLC   0(3,RE),=3X'00'                                                  
         BNE   *+14                                                             
         MVC   0(3,RE),WORK        NOT IN LIST, SO PUT IT IN LIST               
         B     *+12                                                             
         LA    RE,3(RE)            BUMP TO NEXT 3 CHARS CODE IN LIST            
         B     EDIT35                                                           
         MVC   PPPUBACD,WORK       3 CHAR AD SIZE CODE                          
         OI    ESWITCH,1           SOMETHING HAS BEEN ENTERED                   
*                                                                               
EDIT54   TM    WKFLAG,NOSPACE      NO INPUT IN SPACE FIELD?                     
         BZ    EDIT60              VALID SPACE INPUT HAS BEEN ENTERED           
         ST    R2,FULL             SAVE OFF R2                                  
         LA    R6,2                                                             
EDIT55   BAS   RE,NEXTFLD                                                       
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         L     R2,FULL                                                          
         LA    R3,1                                                             
         B     ERROR                                                            
         BCT   R6,EDIT55                                                        
         L     R2,FULL                                                          
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         B     EDIT70              NEXT SET OF SPACE, WIDTH, DEPTH              
*                                                                               
EDIT60   LA    R6,2                                                             
         MVI   PPPUBAEL,PPPUBAEQ   AD SIZE ELEMENT CODE                         
         MVI   PPPUBALN,PPPUBALNQ  AD SIZE ELEMENT LENGTH                       
         MVI   PPPUBAUI,C'I'       AD SIZE UNIT INDICATOR (I=INCH)              
*                                                                               
EDIT65   DS    0X                                                               
         BAS   RE,NEXTFLD                                                       
         LA    R3,1                MISSING INPUT ERR MSG                        
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         BAS   RE,CKWDDP           CHECK WIDTH AND DEPTH IN PAGE                
         BE    *+12                DIMENSION FIELDS (20 SETS)                   
         LA    R3,2                                                             
         B     ERROR                                                            
         BAS   RE,NUMBCONV         CONVERT VALIDATED INPUT TO BINARY            
         BE    *+12                                                             
         LA    R3,2                                                             
         B     ERROR                                                            
         CH    R6,=H'2'                                                         
         BNE   EDIT66                                                           
         MVC   PPPUBAWU,WKINT      TRIM WIDTH (BINARY)                          
         MVC   PPPUBAWN,WKNUMTOR+3                                              
         MVC   PPPUBAWD,WKDENTOR+3                                              
         B     EDIT68                                                           
EDIT66   CH    R6,=H'1'                                                         
         BNE   EDIT67                                                           
         MVC   PPPUBADU,WKINT      TRIM DEPTH (BINARY)                          
         MVC   PPPUBADN,WKNUMTOR+3                                              
         MVC   PPPUBADD,WKDENTOR+3                                              
         B     EDIT68                                                           
EDIT67   DC    H'0'                IMPOSSIBLE                                   
EDIT68   BCT   R6,EDIT65                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CHELLO-COMFACSD)(RF)                                         
         GOTO1 (RF),DMCB,(C'P',=C'PUBFILE'),PUBZREC,ELEAREA,0                   
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   RECLEN,PUBZREC+25                                                
         ZICM  R3,RECLEN,2                                                      
         BCTR  R3,0                                                             
         STH   R3,RECLEN                                                        
*                                                                               
EDIT70   BAS   RE,NEXTFLD                                                       
         DROP  R7                                                               
         BCT   R0,EDIT30                                                        
*                                                                               
EDITXX   DS    0H                                                               
*                                                                               
***********************************************************************         
*                                                                               
UPDATE00 DS    0H                                                               
*                                                                               
***********************************************************************         
*                                                                               
*       PUBADDR = 0'S IF NEW REC TO BE ADDED                                    
*       IF ESWITCH=0 DO NOTHING (NO DATA ENTERED)                               
*       PUBADDR NE 0'S IF REC EXISTS                                            
*       IF ESWITCH=0 FLAG FOR DELETE (DMWRT)                                    
*       ELSE REWRITE RECORD                                                     
*                                                                               
***********************************************************************         
*                                                                               
UPDATE4  DS    0H                                                               
         OC    PUBADDR,PUBADDR     IS IT A NEW ADDRESS RECORD ?                 
         BZ    ADDIT               YES                                          
*                                  NO - RECORD EXISTS                           
         CLI   ESWITCH,0           ANYTHING ENTERED ?                           
         BNE   WRITEIT             YES                                          
         MVC   KEY+27(4),PUBADDR   NO - MUST BE DELETE                          
         MVC   PUBZREC+25(2),RECLEN                                             
         BAS   RE,PUTPUB           REWRITE RECORD                               
*                                                                               
         CLC   RECLEN,=H'33'       SEE IF "EMPTY"                               
         BNE   UPDATOVR            NO - FINISH WORK                             
*                                                                               
         MVI   KEY+25,X'FF'                                                     
         BAS   RE,WRITEPUB         FLAG DIR FOR DELETE                          
         B     UPDATOVR            FINISH WORK                                  
*                                                                               
WRITEIT  DS    0H                                                               
         MVC   KEY+27(4),PUBADDR                                                
         MVC   PUBZREC+25(2),RECLEN                                             
         BAS   RE,PUTPUB                                                        
         CLI   KEY+25,X'FF'        FLAGGED FOR DELETION ?                       
         BNE   UPDATOVR            NO                                           
         MVI   KEY+25,0            TURN OFF                                     
         BAS   RE,WRITEPUB         REWRITE DIR                                  
         B     UPDATOVR            FINISH WORK                                  
*                                                                               
ADDIT    DS    0H                                                               
         CLC   RECLEN,=H'33'       ANY ELEMENTS ?                               
         BH    ADDITOK             YES                                          
         LA    R2,ADSTRWDH         CURSOR TO "TOP"                              
         LA    R3,1                INPUT MISSING MSG                            
         B     ERROR                                                            
ADDITOK  XC    KEY,KEY                                                          
         MVC   KEY(25),PUBZREC                                                  
         MVC   PUBZREC+25(2),RECLEN                                             
         BAS   RE,ADDPUB                                                        
*                                                                               
UPDATOVR DS    0H                                                               
         MVI   ADDCHSW,1                                                        
         B     PUTFLDXX                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
FORMATP  DS    0H                                                               
         CLI   SAVSCRN,X'17'                                                    
         BNE   FMT20                                                            
         CLI   BACT,1              SEE IF ADD                                   
         BNE   FMT50                                                            
         MVI   BYTE2,0             TO GENERATE TURNAROUND                       
         B     EDIT00                                                           
*                                                                               
FMT20    LA    R6,PBLLAST                                                       
         GOTO1 VCALLOV,WORK,(R6),X'D90406F4'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRGERR                                                          
         MVI   SAVSCRN,X'17'                                                    
*                                                                               
FMT50    DS    0H                                                               
         MVI   CLRSCRFG,0                                                       
         TM    LTLIND,X'10'                                                     
         BNZ   PUTFLD00                                                         
*                                                                               
FMT55    FOUT  ADSTRWDH,SPACES,8                                                
         FOUT  ADSTRDPH,SPACES,8                                                
         LA    R2,ADSSPFSH                                                      
         LA    R5,20               TWENTY FIELDS: SPACE, WIDTH, DEPTH           
FMT60    FOUT  (R2),SPACES,17                                                   
         LA    R4,2                TWO REPEATED FIELDS: WIDTH, DEPTH            
FMT70    BAS   RE,NEXTFLD                                                       
         FOUT  (R2),SPACES,8                                                    
         BCT   R4,FMT70                                                         
         BAS   RE,NEXTFLD                                                       
         BCT   R5,FMT60                                                         
*                                                                               
         TM    CLRSCRFG,GOCLR                                                   
         BNO   *+12                                                             
         OI    CLRSCRFG,CLROK                                                   
         B     PUTFLD00                                                         
*                                                                               
         B     PUTFLD99                                                         
*                                                                               
PUTFLD00 DS    0X                                                               
         OI    CLRSCRFG,GOCLR                                                   
         TM    CLRSCRFG,CLROK                                                   
         BO    *+8                                                              
         B     FMT55                                                            
         TM    LTLIND,X'10'                                                     
         BNO   PUTFLDXX                                                         
         LA    R4,PUBZREC+33                                                    
*                                                                               
         MVI   ELCODE,PPPUBTEQ     TRIM ELEM CODE                               
         BAS   RE,GETEL            GET ELEMENT                                  
*****    BE    *+6                                                              
*****    DC    H'0'                TRIM ELEMENT IS REQUIRED                     
         BNE   PUTFLD10            TRIM ELEMENT NOT REQUIRED                    
         USING PPPUBTD,R4                                                       
*                                                                               
         LA    R1,WKTEMP                                                        
         LA    R2,ADSTRWDH                                                      
         MVC   WK2BYTES,PPPUBTWU                                                
         MVC   WKBYTE1,PPPUBTWN                                                 
         MVC   WKBYTE2,PPPUBTWD                                                 
         BAS   RE,PUTFLD20                                                      
*                                                                               
         LA    R1,WKTEMP                                                        
         LA    R2,ADSTRDPH                                                      
         MVC   WK2BYTES,PPPUBTDU                                                
         MVC   WKBYTE1,PPPUBTDN                                                 
         MVC   WKBYTE2,PPPUBTDD                                                 
         BAS   RE,PUTFLD20                                                      
         DROP  R4                                                               
*                                                                               
PUTFLD10 LA    R4,PUBZREC+33                                                    
         USING PPPUBAD,R4                                                       
*                                                                               
         LA    R2,ADSSPFSH         POINT TO FIRST SPACE FIELD                   
         MVI   ELCODE,PPPUBAEQ     NON-BLEED AD SIZE ELEMENT                    
PUTFLD30 BAS   RE,GETEL            GET ELEMENT                                  
         BNE   DONE                                                             
*                                                                               
         GOTO1 =V(PSIZEVAL),DMCB,(1,PPPUBACD),WORK,RR=RELO                      
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                CAN'T FAIL!                                  
         FOUT  (R2),WORK+3,17      AD SIZE DESCRIPTION                          
         BAS   RE,NEXTFLD                                                       
*                                                                               
         LA    R1,WKTEMP                                                        
         MVC   WK2BYTES,PPPUBAWU                                                
         MVC   WKBYTE1,PPPUBAWN                                                 
         MVC   WKBYTE2,PPPUBAWD                                                 
         BAS   RE,PUTFLD20                                                      
*                                                                               
         BAS   RE,NEXTFLD                                                       
         LA    R1,WKTEMP                                                        
         MVC   WK2BYTES,PPPUBADU                                                
         MVC   WKBYTE1,PPPUBADN                                                 
         MVC   WKBYTE2,PPPUBADD                                                 
         BAS   RE,PUTFLD20                                                      
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTEL           POINT TO NEXT ELEMENT                        
         BNE   DONE                                                             
         B     PUTFLD30            SEE IF THERE IS MORE NON-BLEED ELEM          
         DROP  R4                                                               
*                                                                               
         B     DONE                                                             
*                                                                               
*                                                                               
*                                                                               
PUTFLD20 ST    RE,FULL                                                          
         MVC   WKTEMP,SPACES                                                    
         OC    WK2BYTES,WK2BYTES                                                
         BZ    PUTFLD21                                                         
         EDIT  (B2,WK2BYTES),(3,(R1)),ALIGN=LEFT       INTEGER                  
         BAS   RE,PUTFLD90                                                      
         LA    R1,1(R1)                                                         
PUTFLD21 CLI   WKBYTE1,0                                                        
         BE    PUTFLD25            INT ONLY, NO FRACTION                        
         EDIT  (B1,WKBYTE1),(3,(R1)),ALIGN=LEFT        NUMERATOR                
         BAS   RE,PUTFLD90                                                      
         MVI   0(R1),C'/'                                                       
         LA    R1,1(R1)                                                         
         EDIT  (B1,WKBYTE2),(3,(R1)),ALIGN=LEFT        DENOMINATOR              
PUTFLD25 FOUT  (R2),WKTEMP,8                                                    
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
PUTFLD90 LA    R0,0                R1: POINT TO INSERTION POSITION              
PUTFLD91 CLI   0(R1),C' '                                                       
         BE    PUTFLD92                                                         
         CH    R0,=H'8'                                                         
         BNH   *+6                                                              
         DC    H'0'                IMPOSSIBLE!                                  
         LA    R1,1(R1)                                                         
         LA    R0,1(R0)                                                         
         B     PUTFLD91                                                         
PUTFLD92 BR    RE                  NEW INSERTION POINT                          
*                                                                               
*                                                                               
*                                                                               
PUTFLD99 DS    0X                                                               
*                                                                               
*                                                                               
*                                                                               
PUTFLDXX DS    0X                                                               
*                                                                               
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
NEXTFLD  ZIC   R3,0(R2)            R2: POINTS TO CURRENT FIELD                  
         AR    R2,R3                                                            
         BR    RE                  R2 IS NOW POINTING TO NEXT FIELD             
*                                                                               
***********************************************************************         
*                                                                               
CKWDDP   NTR1                      CHECKING VALID WIDTH & DEPTH INPUTS          
*                                                                               
*                                                                               
*                                                                               
*        ON EXIT, VARIABLE "WKINPUT" IS FILLED WITH VALIDATED INPUT             
*        WHICH IS USED TO CONVERT INTO BINARY                                   
*                                                                               
*                                                                               
         MVC   WKBYTE,5(R2)        LENGTH OF INPUT                              
         ZIC   R5,WKBYTE                                                        
         LA    R4,8(R2)                                                         
*                                                                               
         MVI   CKFLAG,0            INITIALIZATION OF WORKING STORAGES           
         XC    WKINPUT,WKINPUT                                                  
         LA    R6,WKINPUT                                                       
*                                                                               
         CLI   5(R2),1                                                          
         BH    CKWD05                                                           
         TM    4(R2),X'08'         X'08' IS VALID NUMBERIC                      
         BNO   CKWDERR                                                          
         MVC   WKINPUT(1),8(R2)                                                 
         B     CKWDXX                                                           
*                                                                               
CKWD05   BCTR  R5,0                                                             
         LA    R4,1(R4)                                                         
         BCT   R5,*-4                                                           
         CLI   0(R4),C'/'          LAST CHAR CAN'T BE FRACTION                  
         BE    CKWDERR                                                          
         LA    R4,8(R2)                                                         
         ZIC   R5,WKBYTE                                                        
*                                                                               
CKWD10   CLI   0(R4),C' '          GETTING RID OF LEADING SP & ZERO             
         BE    CKWD15                                                           
         CLI   0(R4),C'0'                                                       
         BNE   CKWD35                                                           
*                                                                               
CKWD15   LA    R4,1(R4)                                                         
         BCT   R5,CKWD10                                                        
         MVI   WKINPUT,C'0'        INPUT IS ZERO                                
         B     CKWDXX                                                           
*                                                                               
CKWD25   CLI   0(R4),C' '                                                       
         BE    *+12                                                             
         CLI   0(R4),C'-'          DASHES ARE TREATED SAME AS SPACES            
         BNE   CKWD30                                                           
         TM    CKFLAG,FRACTCUR     SPACE AFTER FRACTION IS NO GOOD              
         BO    CKWDERR                                                          
         TM    CKFLAG,SPACECUR     NO NEED FOR CONSECUTIVE SPACES               
         BO    CKWD65                                                           
         OI    CKFLAG,SPACEBAR     HAVE A SPACE IN INPUT                        
         OI    CKFLAG,SPACECUR     CURRENT CHAR IS A SPACE                      
         NI    CKFLAG,X'FF'-DIGITS                                              
         NI    CKFLAG,X'FF'-LEADZERO                                            
         NI    CKFLAG,X'FF'-FRACTCUR                                            
         B     CKWD60                                                           
*                                                                               
CKWD30   CLI   0(R4),C'/'                                                       
         BNE   CKWD35                                                           
         TM    CKFLAG,FRACTSYM                                                  
         BO    CKWDERR                                                          
         OI    CKFLAG,FRACTSYM                                                  
         OI    CKFLAG,FRACTCUR                                                  
         B     CKWD60                                                           
*                                                                               
CKWD35   CLI   0(R4),C'0'                                                       
         BE    CKWD40                                                           
         BL    CKWDERR                                                          
         CLI   0(R4),C'9'                                                       
         BH    CKWDERR                                                          
*                                                                               
         TM    CKFLAG,FRACTSYM                                                  
         BZ    *+12                                                             
         TM    CKFLAG,SPACECUR                                                  
         BO    CKWDERR             NO NUMBERS AFTER FRACTION                    
*                                                                               
         OI    CKFLAG,DIGITS       CONTAINS A SIGNIFICANT DIGIT                 
         NI    CKFLAG,X'FF'-LEADZERO                                            
         NI    CKFLAG,X'FF'-SPACECUR                                            
         NI    CKFLAG,X'FF'-FRACTCUR                                            
         B     CKWD60                                                           
*                                                                               
CKWD40   TM    CKFLAG,DIGITS       HAS A SIGNIFICANT DIGIT?                     
         BO    CKWD60              YES                                          
         TM    CKFLAG,LEADZERO                                                  
         BO    CKWD65              NO NEED FOR CONSECUTIVE ZEROES               
         OI    CKFLAG,LEADZERO                                                  
         NI    CKFLAG,X'FF'-DIGITS                                              
*                                                                               
CKWD60   DS    0X                                                               
         MVC   0(1,R6),0(R4)                                                    
         LA    R6,1(R6)                                                         
CKWD65   LA    R4,1(R4)                                                         
         BCT   R5,CKWD25                                                        
*                                                                               
         TM    CKFLAG,FRACTSYM     CAN'T HAVE SPACE IN INPUT                    
         BO    CKWDXX              IF THERE'S NO FRATION INPUT                  
         TM    CKFLAG,SPACEBAR                                                  
         BO    CKWDERR                                                          
*                                                                               
CKWDXX   SR    RB,RB                                                            
CKWDERR  LTR   RB,RB                                                            
*                                                                               
CKWDDPX  XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
NUMBCONV NTR1                      CONVERT INPUT TO BINARY                      
*                                                                               
*        USES "WKINPUT" WHICH GOTTEN FROM CKWDDP SUB-ROUTINE                    
*        ON EXIT, WKINT    IS FILLED WITH BINARY INTEGER                        
*                 WKNUMTOR IS FILLED WITH BINARY NUMERATOR                      
*                 WKDENTOR IS FILLED WITH BINARY NUMERATOR                      
*                                                                               
         SR    R2,R2                                                            
         XC    WKTEMP,WKTEMP                                                    
         XC    WKINT,WKINT         INTEGER IN BINARY                            
         XC    HALF,HALF           INT CONVERTED FROM IMPROPER FRACT            
         XC    WKNUMTOR,WKNUMTOR   NUMERATOR IN BINARY                          
         XC    WKDENTOR,WKDENTOR   DENOMINATOR IN BINARY                        
         LA    R4,WKTEMP                                                        
         LA    R5,WKINPUT                                                       
         MVI   WKFLAG,0                                                         
*                                                                               
NCONV10  CH    R2,=H'17'                                                        
         BNH   *+6                                                              
         DC    H'0'                CAN'T GO BEYOUND WKINPUT!!                   
         CLI   0(R5),C'/'                                                       
         BE    NCONV30             DENOMINATOR BEGINS                           
         CLI   0(R5),C' '                                                       
         BE    NCONV40             FRACTION PORTION BEGINS                      
         CLI   0(R5),C'-'                                                       
         BE    NCONV40             FRACTION PORTION BEGINS                      
         CLI   0(R5),0                                                          
         BE    NCONV50             END OF WKINPUT                               
         MVC   0(1,R4),0(R5)                                                    
         LA    R2,1(R2)            DIGIT(S) COUNTER                             
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     NCONV10                                                          
*                                                                               
NCONV30  OI    WKFLAG,DENTORBG                                                  
         B     NCONV50                                                          
*                                                                               
NCONV40  OI    WKFLAG,FRACTBG                                                   
*                                                                               
NCONV50  BAS   RE,PACKIT                                                        
*                                                                               
         TM    WKFLAG,DENTORBG     DENOMINATOR IS COMING UP                     
         BO    NCONV60                                                          
         TM    WKFLAG,FRACTBG      FRACTION IS COMING UP                        
         BO    NCONV80                                                          
*                                                                               
         B     NCONVXXX            INPUT IS INTEGER ONLY                        
*                                                                               
NCONV60  DS    0X                  GETTING DENOMINATOR                          
         CLI   0(R5),C'/'                                                       
         BE    *+6                                                              
         DC    H'0'                MUST BE A FRACTION SYMBOL                    
         LA    R5,1(R5)                                                         
*                                                                               
         ST    R4,WKNUMTOR         BINARY NUMERATOR STORED                      
         SR    R2,R2                                                            
         XC    WKTEMP,WKTEMP                                                    
         LA    R4,WKTEMP                                                        
*                                                                               
NCONV65  CH    R2,=H'17'                                                        
         BNH   *+6                                                              
         DC    H'0'                CAN'T GO BEYOUND WKINPUT!!                   
         CLI   0(R5),0                                                          
         BE    NCONV70             END OF WKINPUT                               
         MVC   0(1,R4),0(R5)                                                    
         LA    R2,1(R2)            DIGIT(S) COUNTER                             
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     NCONV65                                                          
*                                                                               
NCONV70  DS    0X                                                               
         OC    WKTEMP,WKTEMP                                                    
         BNZ   NCONV75                                                          
NCONV71  L     R3,WKNUMTOR                                                      
         CH    R3,=H'0'                                                         
         BNE   NCONVERR            FRACTION IS NOT 0/0, BUT X/0                 
         B     NCONVXX             0/0 IS ALLOWED                               
*                                                                               
NCONV75  BAS   RE,PACKIT                                                        
         ST    R4,WKDENTOR         SAVE OFF DENOMINATOR                         
         CH    R4,=H'0'                                                         
         BE    NCONV71                                                          
         L     R3,WKNUMTOR         GET NUMERATOR                                
         CR    R3,R4               NUMERATOR VS. DENOMINATOR                    
         BE    NCONV79                                                          
         BL    NCONVXX                                                          
*                                  TAKING CARE OF IMPROPER FRACTION             
         SR    R1,R1                                                            
         SR    R3,R4                                                            
         LA    R1,1(R1)                                                         
         CR    R3,R4                                                            
         BNL   *-8                                                              
         CH    R1,=H'256'                                                       
         BH    NCONVERR            CAN'T BE MORE THAN 1 BYTE BINARY             
         STH   R1,HALF             INT CONVERTED FROM IMPROPER FRACT            
         ST    R3,WKNUMTOR         STORE NEW, REDUCED NUMERATOR                 
         CH    R3,=H'0'                                                         
         BNE   *+10                                                             
         XC    WKDENTOR,WKDENTOR   IMPROPER FRACTION IS AN INTEGER              
         B     NCONVXX                                                          
*                                                                               
NCONV79  SR    R1,R1               NUMERATOR=DENOMINATOR (1/1)                  
         LH    R1,HALF                                                          
         LA    R1,1(R1)                                                         
         STH   R1,HALF             ADD 1 TO INTEGER PORTION                     
         XC    WKNUMTOR,WKDENTOR                                                
         XC    WKDENTOR,WKDENTOR                                                
         B     NCONVXX                                                          
*                                                                               
*                                                                               
*                                                                               
NCONV80  DS    0X                  GETTING FRACTION                             
         CLI   0(R5),C' '                                                       
         BE    NCONV81                                                          
         CLI   0(R5),C'-'                                                       
         BE    NCONV81                                                          
         DC    H'0'                MUST BE A SPACE OR A DASH                    
NCONV81  LA    R5,1(R5)                                                         
         STH   R4,WKINT            SAVE OFF INTEGER GOTTEN FROM PACKIT          
         SR    R2,R2                                                            
         XC    WKTEMP,WKTEMP                                                    
         LA    R4,WKTEMP                                                        
         NI    WKFLAG,X'FF'-FRACTBG                                             
         B     NCONV10                                                          
*                                                                               
*                                                                               
*                                                                               
PACKIT   DS    0X                  R2=NUMBER OF BYTE(S) TO BE PACKED            
         XC    DUB,DUB             R4=BINARY VERSION OF PACKED NUMBER           
         CH    R2,=H'0'            PACKING FROM WKTEMP                          
         BH    *+6                                                              
         DC    H'0'                CAN'T PACK NEGATIVE LENGTH!                  
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WKTEMP(0)                                                    
         CVB   R4,DUB                                                           
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
NCONVXX  DS    0X                                                               
         ZICM  R4,WKINT,2                                                       
         AH    R4,HALF                                                          
NCONVXXX STH   R4,WKINT                                                         
         CH    R4,=H'255'                                                       
         BH    NCONVERR            INTEGER IS 2 DIGITS                          
         L     R4,WKNUMTOR                                                      
         CH    R4,=H'255'                                                       
         BH    NCONVERR            NUMERATOR IS 2 DIGITS                        
         L     R4,WKDENTOR                                                      
         CH    R4,=H'255'                                                       
         BH    NCONVERR            DENOMINATOR IS 2 DIGITS                      
*                                                                               
         SR    RB,RB                                                            
NCONVERR LTR   RB,RB                                                            
*                                                                               
NCONVX   XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
WKBYTE   DS    CL1                                                              
WKINPUT  DS    XL17                                                             
RECLEN   DS    H                   RECORD LENGTH                                
WKINT    DS    H                   INTEGER PORTION OF INPUT                     
WKNUMTOR DS    F                   NUMERATOR                                    
WKDENTOR DS    F                   DENOMINATOR                                  
WKFRACT  DS    CL8                 REDUCED FRACTION IN CHAR FORMAT              
WKFRCODE DS    XL1                 FRACTION CODE FROM FRACTION TABLE            
WKASCODE DS    CL3                 AD SIZE CODE FROM PSIZETAB                   
*                                                                               
CODELIST DS    CL63                LIST OF 3 CHAR CODES (3X20+3)                
*                                                                               
WKTEMP   DS    XL8                                                              
WK2BYTES DS    XL2                                                              
WKBYTE1  DS    XL1                                                              
WKBYTE2  DS    XL1                                                              
*                                                                               
CKFLAG   DS    XL1                                                              
FRACTSYM EQU   X'80'               CONTAIN FRACTION SYMBOL ALREADY?             
SPACEBAR EQU   X'40'               CONTAIN A SPACE IN INPUT?                    
LEADZERO EQU   X'20'               CONTAIN A LEADING ZERO?                      
DIGITS   EQU   X'10'               CONTAIN A DIGIT?                             
FRACTCUR EQU   X'08'               CURRENT CHAR IS A FRACTION SYMBOL?           
SPACECUR EQU   X'04'               CURRENT CHAR IS A SPACE?                     
*                                                                               
WKFLAG   DS    XL1                 WORKING FLAG FOR GENERAL PURPOSES            
NOSPACE  EQU   X'80'               NO INPUT IN SPACE INPUT                      
*                                                                               
DENTORBG EQU   X'80'               DENOMINATOR BEGINS                           
FRACTBG  EQU   X'40'               FRACTION BEGINS                              
*                                                                               
CLRSCRFG DS    XL1                 CLEAR SCREEN                                 
GOCLR    EQU   X'80'               GO CLEAR SCREEN                              
CLROK    EQU   X'40'               SCREEN IS CLEARED                            
*                                                                               
LTLIND   DS    CL1                                                              
ELCODE   DS    CL1                                                              
ASWITCH  DS    CL1                                                              
ESWITCH  DS    CL1                                                              
ADDCHSW  DS    CL1                                                              
SVINBTS  DS    CL1                                                              
AFRATAB  DS    A                                                                
ASIZTAB  DS    A                                                                
*                                                                               
FLDINV   EQU   2                                                                
COMBERR  EQU   112                                                              
REPERR   EQU   122                                                              
*                                                                               
ELCNT    DC    H'0'                                                             
VIRGERR  DC    H'0'                                                             
SPACES   DC    80C' '                                                           
DMWORK1  DS    12D                                                              
SAVERE   DS    F                                                                
SAVEKEY  DS    CL32                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
*        GET ELEMENT IN RECORD                                                  
*                                                                               
*                                  ELCODE IS ELEMENT CODE                       
*                                  R4 IS POINTING TO RECORD                     
GETEL    DS    0X                                                               
         LA    R0,999              FOR CONDITION CODE                           
GETEL10  CLC   ELCODE,0(R4)                                                     
         BE    YESELEM                                                          
         ZIC   R3,1(R4)            ELEMENT LENGTH                               
         AR    R4,R3               BUMP TO NEXT ELEM                            
         CLI   0(R4),0             IS IT THE END?                               
         BE    NOELEM              YES                                          
         B     GETEL10                                                          
*                                                                               
*                                                                               
*                                                                               
NEXTEL   DS    0X                                                               
         LA    R0,999              FOR CONDITION CODE                           
         ZIC   R3,1(R4)            ELEMENT LENGTH                               
         AR    R4,R3               BUMP TO NEXT ELEM                            
         CLI   0(R4),0             IS IT THE END?                               
         BE    NOELEM              YES                                          
YESELEM  SR    R0,R0                                                            
NOELEM   LTR   R0,R0                                                            
         BR    RE                                                               
*                                                                               
***********************************************************************         
*                                                                               
         EJECT                                                                  
       ++INCLUDE PUGENEROL                                                      
*                                                                               
         LTORG                                                                  
*                                                                               
ELEAREA  DS    XL255                                                            
*                                                                               
         DS    0F                                                               
PUBIO    DS    4000C               MAX RECORD SIZE IS 4000                      
*                                                                               
         ORG   PUBIO                                                            
         EJECT                                                                  
       ++INCLUDE PPUBZREC                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE PUGENOLD                                                       
*                                                                               
         ORG   IOAREA                                                           
         DS    CL500                                                            
         DS    D                                                                
ASYSPARS DS    A                                                                
         DS    A                                                                
         DS    CL50                SPARE WORK AREA                              
*                                                                               
         DS    CL30                                                             
*                                                                               
         SPACE 2                                                                
*                                                                               
       ++INCLUDE FLDIND            EQUS FOR FLD INPUT/OUTPUT INDICATOR          
         EJECT                                                                  
       ++INCLUDE DDCOMFACS         COMMON FACITITIES                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPUBFFD                                                       
         ORG   PBLLAST                                                          
       ++INCLUDE PPPUBF4D          NON-BLEED AD PAGE SCR                        
*                                                                               
         ORG   T406FFD                                                          
         DS    CL16                                                             
BMED     DS    CL1                                                              
BACT     DS    CL1                                                              
BSCR     DS    CL1                                                              
OLNUM    DS    CL1                                                              
PUBADDR  DS    F                                                                
LTLADDR  DS    F                                                                
BPUB     DS    CL6                                                              
BCLT     DS    CL3                                                              
BDIV     DS    CL3                                                              
BDATE    DS    CL3                                                              
APROF    DS    CL1                                                              
SAVSCRN  DS    CL1                                                              
APROF13  DS    CL1                                                              
BCODE    DS    CL3                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011PPPUB17   01/29/99'                                      
         END                                                                    
