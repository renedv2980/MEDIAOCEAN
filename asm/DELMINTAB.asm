*          DATA SET DELMINTAB  AT LEVEL 003 AS OF 10/06/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMINTA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE LOADER                                                                 
*INCLUDE KHDUMMY                                                                
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE PDUMPER                                                                
         TITLE 'LOCAL MONTHLY INTAB CONVERSION'                                 
*****************************************************************               
* SEP/2011: CREATE INDIVIDUAL DAY INTAB RECORD, WITH ALL 255                    
*           DEMO CATEGORIES.                                                    
*   INPUT : NIELESEN INDIVIDUAL DAY INTAB RECORDS                               
*   OUTPUT: INTERNAL INTAB RECORDS. F/M/S/ IS -ITA-                             
*   NOTE!!: THIS IS A STAND ALONE CONVERSION MODULE THAT CALLS                  
*           SPGETIUN TO POPULATE ALL TTN/IUN CELLS FROM THE CORE                
*           CELLS.                                                              
*****************************************************************               
*                                                                               
DELMINT  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*DELMINT,=V(REGSAVE),R8                                        
*                                                                               
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         B     INIT                                                             
*                                                                               
STXTAB   DS    0H                                                               
         DC    A(DELMINT)                                                       
         DC    V(DUMMY)                                                         
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
***********************************************************************         
INIT     LA    RC,INREC                                                         
         USING LMDSECT,RC                                                       
         LA    R2,WORKREC                                                       
         USING DRKEY,R2                                                         
*                                                                               
READCARD GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    CARDX                                                            
*                                                                               
         CLC   =C'GETIUN=',CARD    BEN WROTE A GETIUN ROUTINE                   
         BE    *+6                 TO POPULATE ALL CELLS                        
         DC    H'0'                BASED ON THE CORE CELLS                      
         MVC   DUB,CARD+7                                                       
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETIUN,4(R1)                                                    
         B     READCARD                                                         
*                                                                               
CARDX    OPEN  (IN1,(INPUT))                                                    
         OPEN  (OUT,(OUTPUT))                                                   
*                                                                               
LMINTAB  GET   IN1,(RC)                                                         
         BAS   RE,CALCTEEN         CREATE M AND W 12-17                         
         CLC   W_KDTYPE,PREVDTYP                                                
         BE    *+14                                                             
         MVI   DAY#,1                                                           
         MVC   PREVDTYP,W_KDTYPE                                                
*                                                                               
LMINT10  MVI   DRCODE,DRITBEQU                                                  
         MVI   DRMEDIA,C'T'                                                     
         MVI   DRSRC,C'A'                                                       
*                                                                               
         PACK  DUB,W_KMKTC                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,DRKMKT                                                      
***********************************************************************         
* BOOK                             INTBOOK                                      
***********************************************************************         
LMBOOK   PACK  DUB,W_KRPYR                                                      
         CVB   RE,DUB                                                           
         CH    RE,=H'1999'                                                      
         BH    *+12                                                             
         SH    RE,=H'1900'                                                      
         B     *+8                                                              
         SH    RE,=H'2000'                                                      
         CHI   RE,27                Y2K FIX--IF 2-CHAR YEAR < 27,               
         BH    *+8                                                              
         AHI   RE,100                MAKE IT A 21ST CENTURY YEAR                
         STC   RE,DRBOOK                                                        
         PACK  DUB,W_KRPRD                                                      
         CVB   RE,DUB                                                           
         STC   RE,DRBOOK+1                                                      
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,DRKMKT                                                      
         CVD   RE,DUB                                                           
         UNPK  DRSTAT(4),DUB                                                    
         OI    DRSTAT+3,X'F0'                                                   
         MVI   DRSTAT+4,C'I'                                                    
***********************************************************************         
* MARKET MEASUREMENT TYPE                                                       
***********************************************************************         
LMSTYP   MVC   DRSTYP,W_MHQCMETH                                                
***********************************************************************         
* BOOKTYPE                         DRBTYP                                       
***********************************************************************         
LMBTYP   MVC   LMBTKEY,SPACE                                                    
         MVC   LMBTKEY(1),W_KPBACK    S/O/1/3/7                                 
*                                                                               
         CLI   LMBTKEY,C'1'        LIVE+1 IS STANDARD FOR DIARY                 
         BNE   *+8                                                              
         MVI   LMBTKEY,C'7'        LIVE+7 IS THE STANDARD BOOKTYPE KEY          
*                                                                               
         CLI   W_MHQCMETH,W_COLLECTION_METHOD_LPM_PRELIM                        
         BNE   *+8                                                              
         MVI   LMBTKEY,C'P'                                                     
*  -----------------------------------------------------------                  
*  FIELD #2 IS DATA TYPE: ALL DATA TYPES ARE MUTUALLY EXCLUSIVE                 
*  THEY ARE NOT EXPECTED TO CO-EXIST.  THERE ARE NO BOOKTYPE                    
*  SUPPORT FOR THE COMBINATION OF ANY OF THE DATATYPES BESIDES                  
*  STANDARD. EX: THERE WON'T BE BLACK+HISPANIC                                  
*  -----------------------------------------------------------                  
         CLI   W_KRSRVC,C'2'       NHSI?                                        
         BNE   *+12                                                             
         MVI   LMBTKEY+1,C'H'                                                   
         B     LMBT10                                                           
*                                                                               
         OC    W_KSUBSI,SPACE      TURN TO UPPER-CASE                           
         CLC   W_KSUBSI,=C'AF-AM'                                               
         BNE   *+12                                                             
         MVI   LMBTKEY+1,C'B'                                                   
         B     LMBT10                                                           
*                                                                               
         CLC   W_KSUBSI,=C'HISP '                                               
         BNE   *+12                                                             
         MVI   LMBTKEY+1,C'H'                                                   
         B     LMBT10                                                           
*                                                                               
*  -----------------------------------------------------------                  
LMBT10   CLI   W_KSAMTY,C'2'       TOTAL DMA OR WIRED                           
         BNE   *+8                                                              
         MVI   LMBTKEY+3,C'W'                                                   
*                                                                               
         LA    R1,BTYPTAB                                                       
LMBT20   CLI   0(R1),X'FF'                                                      
         BE    SKIP                                                             
         CLC   LMBTKEY,0(R1)                                                    
         BE    *+12                                                             
         LA    R1,L'BTYPTAB(R1)                                                 
         B     LMBT20                                                           
         MVC   DRBTYP,L'BTYPTAB-1(R1)                                           
         B     LMBTX                                                            
*                                                                               
LMBTKEY  DS    XL(L'BTYPTAB-1)                                                  
                                                                                
LMBTX    DS    0H                                                               
***********************************************************************         
* DAY/TIME                         INTDAY,INTSQH,INTEQH                         
***********************************************************************         
LMDT     DS    0H                                                               
         SR    R0,R0                                                            
         ZIC   R1,DAY#                                                          
         LA    RF,7                                                             
         DR    R0,RF                                                            
         CHI   R0,0                                                             
         BNE   *+12                                                             
         LA    R0,7                                                             
         SHI   R1,1                                                             
         SHI   R0,1                                                             
         LA    RE,DAY#TAB                                                       
         AR    RE,R0                                                            
         ZIC   RF,0(RE)                                                         
         MHI   R1,7                                                             
         AR    RF,R1                                                            
         STC   RF,DRHIGHD                                                       
*                                                                               
         ZIC   RE,DAY#                                                          
         AHI   RE,1                                                             
         STC   RE,DAY#                                                          
***********************************************************************         
* INTABS                                                                        
***********************************************************************         
         LA    R4,CINT2INT                                                      
         LA    R6,DRFRSTEL                                                      
         LA    R7,W_QDRMTROA                                                    
         BAS   RE,CNVRTOI                                                       
*                                                                               
         LA    R1,WORKREC                                                       
         ST    R1,DBAREC                                                        
         GOTO1 VGETIUN,DMCB,DBLOCK,0,0                                          
*                                                                               
LMOR     LA    RE,OUTREC                                                        
         LA    RF,2000                                                          
         XCEF                                                                   
         MVC   OUTREC(23),WORKREC                                               
         LA    R6,OUTREC                                                        
*                                                                               
         BAS   RE,SLOTDEM                                                       
         ZICM  RE,DRRLEN-DRKEY(R6),2                                            
         AHI   RE,4                                                             
         STH   RE,OUTLEN                                                        
         PUT   OUT,OUTLEN                                                       
         LA    RE,WORKREC                                                       
         LA    RF,2000                                                          
         XCEF                                                                   
         B     LMINTAB                                                          
         EJECT                                                                  
***********************************************************************         
GUIDE    NTR1                                                                   
         LA    R1,DRFRSTEL                                                      
         LA    RF,1                                                             
G10      CHI   RF,255                                                           
         BE    GX                                                               
         EDIT  (RF),(4,(R1))                                                    
         LA    R1,4(R1)                                                         
         AHI   RF,1                                                             
         B     G10                                                              
*                                                                               
GX       XIT1                                                                   
***********************************************************************         
CALCTEEN NTR1                                                                   
         ICM   R1,15,W_QDRM1214                                                 
         ICM   R2,15,W_QDRM1517                                                 
         AR    R1,R2                                                            
         STCM  R1,15,W_QDRM1214                                                 
         XC    W_QDRM1517,W_QDRM1517                                            
*                                                                               
         ICM   R1,15,W_QDRF1214                                                 
         ICM   R2,15,W_QDRF1517                                                 
         AR    R1,R2                                                            
         STCM  R1,15,W_QDRF1214                                                 
         XC    W_QDRF1517,W_QDRF1517                                            
*                                                                               
CTX      XIT1                                                                   
***********************************************************************         
*        R4  CINT2INT (INTABS -> BUFFER) DISPLACEMENT TABLE                     
*        R6: BUFFER                                                             
*        R7  INTABS                                                             
***********************************************************************         
CNVRTOI  NTR1                                                                   
         SHI   R6,4                                                             
         SHI   R7,4                                                             
CNVRTOI1 CLI   0(R4),X'FF'                                                      
         BE    CNVRTOIX                                                         
         ZIC   R5,0(R4)            GET RS FIELD NUMBER                          
         MHI   R5,4                ADJUST FOR FLD LEN                           
         AR    R5,R7                                                            
         L     RF,0(R5)                                                         
CNVRTOI4 ZIC   R5,1(R4)            GET INT FIELD NUMBER                         
         MH    R5,=H'4'            ADJUST FOR FLD LEN                           
         AR    R5,R6                                                            
CNVRTOI5 ST    RF,0(R5)            SAVE IT                                      
         LA    R4,2(R4)            NEXT FIELD                                   
         B     CNVRTOI1                                                         
*                                                                               
CNVRTOIX XIT1                                                                   
***********************************************************************         
SLOTDEM  NTR1                                                                   
         LA    R2,DRFRSTEL         R2 POINTS AT BUFFER                          
         LA    R3,CATELTAB                                                      
         LA    R4,ELEM                                                          
         USING ITBELD,R4                                                        
SD10     MVI   ITBELCO,ITBELQ                                                   
         ZIC   RE,0(R3)                                                         
         ZIC   RF,1(R3)                                                         
         STC   RE,ITBELLO                                                       
         STC   RF,ITBELHI                                                       
         SR    RF,RE               RF CONTAINS REVERSE COUNTER                  
         AHI   RF,1                                                             
*                                                                               
         LA    R1,ITBVALS                                                       
         LR    RE,R1               RE IS THE START OF THE ELEMENT               
SD20     MVC   0(L'ITBVALS,R1),0(R2)                                            
         LA    R1,L'ITBVALS(R1)                                                 
         LA    R2,L'ITBVALS(R2)                                                 
         BCT   RF,SD20                                                          
         SR    R1,RE                                                            
         AHI   R1,4                                                             
         STC   R1,ITBELLEN                                                      
         GOTO1 =V(HELLO),DMCB,(C'P',DEMFIL),OUTREC,ELEM,0,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    ELEM,ELEM                                                        
         LA    R3,L'CATELTAB(R3)                                                
         CLC   =X'FFFF',0(R3)                                                   
         BNE   SD10                                                             
         MVC   ELEM,=X'5E07C9E3C16C0100'                                        
         GOTO1 =V(HELLO),DMCB,(C'P',DEMFIL),OUTREC,ELEM,0,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SDX      XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
MORET    DS    0H'0'                                                            
ENDJOB   CLOSE (IN1,REWIND)                                                     
         CLOSE (OUT)                                                            
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XBASE                                                                  
SKIP     B     LMINTAB                                                          
         LTORG                                                                  
         EJECT                                                                  
SPACE    DC    CL15' '                                                          
PREVDTYP DS    XL(W_KDTYPLQ)                                                    
DAY#     DC    X'00'                                                            
DUB      DS    D                                                                
DMCB     DS    6F                                                               
CARD     DS    CL80                                                             
VGETIUN  DS    V                                                                
DMELCODE EQU   X'31'                                                            
DEMFIL   DC    CL7'DEMFIL'                                                      
WORK     DS    CL40                                                             
*                                                                               
***********************************************************************         
CATELTAB DS    0AL2                                                             
         DC    AL1(1),AL1(50)                                                   
         DC    AL1(51),AL1(100)                                                 
         DC    AL1(101),AL1(150)                                                
         DC    AL1(151),AL1(200)                                                
         DC    AL1(201),AL1(254)                                                
         DC    X'FFFF'                                                          
DAY#TAB  DS    0AL1                                                             
         DC    AL1(4)                                                           
         DC    AL1(5)                                                           
         DC    AL1(6)                                                           
         DC    AL1(7)                                                           
         DC    AL1(1)                                                           
         DC    AL1(2)                                                           
         DC    AL1(3)                                                           
***********************************************************************         
*** SEAN FEB/11: COMMENTED OUT PRELIMINARY, EXCLUSION AND "LIVE ONLY -          
***              WIRED PARENT ONLY DATA."                                       
***              SPOKE TO MARIA, WE AGREE IT'S BETTER TO WAIT FOR DATA          
***              NOTIFICATION AND REACT TO IT ACCORDINGLY.                      
***********************************************************************         
BTYPTAB  DS    0CL5                                                             
*       :KEY:  1: STREAM                                                        
*       :KEY:  2: DATA TYPE                                                     
*       :KEY:  3: BROADCAST OR CABLE                                            
*       :KEY:  4: TOTAL DMA OR WIRED                                            
*       :KEY:  5: ASSIGNED BOOKTYPE                                             
*  STANDARD                                                                     
         DC    C'O',C' ',C' ',C' ',C'L'                                         
         DC    C'S',C' ',C' ',C' ',AL1(BOOKTYPE_LS)                             
         DC    C'3',C' ',C' ',C' ',AL1(BOOKTYPE_L3)                             
         DC    C'7',C' ',C' ',C' ',X'00'                                        
***      DC    C'P',C' ',C' ',C' ',C'P'                                         
         DC    C'O',C' ',C' ',C'W',C'Z'                                         
         DC    C'S',C' ',C' ',C'W',AL1(BOOKTYPE_WS)                             
         DC    C'3',C' ',C' ',C'W',AL1(BOOKTYPE_W3)                             
         DC    C'7',C' ',C' ',C'W',C'W'                                         
***      DC    C'P',C' ',C' ',C'W',C'Y'                                         
*  CABLE                                                                        
         DC    C'O',C' ',C'C',C' ',C'U'                                         
         DC    C'S',C' ',C'C',C' ',AL1(BOOKTYPE_LS)                             
         DC    C'3',C' ',C'C',C' ',AL1(BOOKTYPE_C3)                             
         DC    C'7',C' ',C'C',C' ',C'C'                                         
***      DC    C'P',C' ',C'C',C' ',C'C'                                         
         DC    C'O',C' ',C'C',C'W',C'Z'                                         
         DC    C'S',C' ',C'C',C'W',AL1(BOOKTYPE_WS)                             
         DC    C'3',C' ',C'C',C'W',AL1(BOOKTYPE_W3)                             
         DC    C'7',C' ',C'C',C'W',C'W'                                         
***      DC    C'P',C' ',C'C',C'W',C'Y'                                         
*  HISPANIC                                                                     
         DC    C'O',C'H',C' ',C' ',C'J'                                         
         DC    C'O',C'H',C'C',C' ',C'J'                                         
         DC    C'S',C'H',C' ',C' ',AL1(BOOKTYPE_HS)                             
         DC    C'S',C'H',C'C',C' ',AL1(BOOKTYPE_HS)                             
         DC    C'3',C'H',C' ',C' ',AL1(BOOKTYPE_H3)                             
         DC    C'3',C'H',C'C',C' ',AL1(BOOKTYPE_H3)                             
         DC    C'7',C'H',C' ',C' ',C'H'                                         
         DC    C'7',C'H',C'C',C' ',C'H'                                         
***      DC    C'P',C'H',C' ',C' ',C'I'                                         
***      DC    C'P',C'H',C'C',C' ',C'I'                                         
*  BLACK                                                                        
         DC    C'7',C'B',C' ',C' ',C'B'                                         
         DC    C'7',C'B',C'C',C' ',C'B'                                         
*  PARENT ONLY                                                                  
         DC    C'O',C'A',C' ',C' ',C'Q'                                         
         DC    C'S',C'A',C' ',C' ',AL1(BOOKTYPE_QS)                             
         DC    C'3',C'A',C' ',C' ',AL1(BOOKTYPE_Q3)                             
         DC    C'7',C'A',C' ',C' ',C'A'                                         
*** THE WIRED PARENT ONLY LIVE ONLY DATA WAS NEVER LOADED                       
*** BECAUSE THIS ENTRY WAS MISSING THE "W" IN THE 3RD COLUMN                    
*** MDAS TELLS ME WE CAN LEAVE IT OUT, IT WAS NEVER RELEASED                    
*** WITH THE VIP.                                                               
***      DC    C'O',C'A',C' ',C' ',C'S'                                         
         DC    C'S',C'A',C' ',C'W',AL1(BOOKTYPE_SS)                             
         DC    C'3',C'A',C' ',C'W',AL1(BOOKTYPE_S3)                             
         DC    C'7',C'A',C' ',C'W',C'R'                                         
*  EXCLUSIONS                                                                   
*        DC    C'7',C'X',C' ',C' ',C'X'                                         
*        DC    C'7',C'X',C'C',C' ',C'X'                                         
*  OLYMPIC EXCLUSIONS                                                           
         DC    C'O',C'O',C' ',C' ',AL1(BOOKTYPE_OL)                             
         DC    C'O',C'O',C'C',C' ',AL1(BOOKTYPE_OL)                             
         DC    C'S',C'O',C' ',C' ',AL1(BOOKTYPE_OS)                             
         DC    C'S',C'O',C'C',C' ',AL1(BOOKTYPE_OS)                             
         DC    C'7',C'O',C' ',C' ',C'O'                                         
         DC    C'7',C'O',C'C',C' ',C'O'                                         
*  TRADE                                                                        
         DC    C'7',C'T',C' ',C' ',C'T'                                         
         DC    C'7',C'T',C'C',C' ',C'T'                                         
*  METRO                                                                        
         DC    C'7',C'M',C' ',C' ',C'M'                                         
         DC    C'7',C'M',C'C',C' ',C'M'                                         
         DC    X'FF'                                                            
***********************************************************************         
***********************************************************************         
* TABLE TO CONVERT INTABS TO BUFFER                                             
CINT2INT DS    0C                                                               
         DC    AL1(RRMETROA,ORMETROA)                                           
         DC    AL1(RRMETROB,ORMETROB)                                           
         DC    AL1(RRHOMES,ORHOMES)                                             
         DC    AL1(RRV25,ORV25)                                                 
         DC    AL1(RRV611,ORV611)                                               
         DC    AL1(RRM1217,ORM1217)                                             
         DC    AL1(RRW1217,ORW1217)                                             
         DC    AL1(RRM1820,ORM1820)                                             
         DC    AL1(RRM2124,ORM2124)                                             
         DC    AL1(RRM2534,ORM2534)                                             
         DC    AL1(RRM3549,ORM3549)                                             
         DC    AL1(RRM5054,ORM5054)                                             
         DC    AL1(RRM5564,ORM5564)                                             
         DC    AL1(RRM65O,ORM65O)                                               
         DC    AL1(RRW1820,ORW1820)                                             
         DC    AL1(RRW2124,ORW2124)                                             
         DC    AL1(RRW2534,ORW2534)                                             
         DC    AL1(RRW3549,ORW3549)                                             
         DC    AL1(RRW5054,ORW5054)                                             
         DC    AL1(RRW5564,ORW5564)                                             
         DC    AL1(RRW65O,ORW65O)                                               
         DC    AL1(RRWWRK,ORWWRK)                                               
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=RRECL,                                            X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
*                                                                               
OUT      DCB   DDNAME=OTAPE,                                           X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=02000,                                            X        
               BLKSIZE=20000,                                          X        
               MACRF=PM                                                         
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
INREC    DS    CL(RRECL)                                                        
ELEM     DS    XL255                                                            
WORKREC  DS    CL2000                                                           
OUTLEN   DS    F                                                                
OUTREC   DS    CL2000                                                           
         EJECT                                                                  
* INTERIM RECORD DISPLACEMENTS                                                  
ORMETROA EQU   2                                                                
ORMETROB EQU   3                                                                
ORHOMES  EQU   1                                                                
ORV25    EQU   121                                                              
ORV611   EQU   123                                                              
ORM1217  EQU   75                                                               
ORM1820  EQU   112                                                              
ORM2124  EQU   113                                                              
ORM2534  EQU   96                                                               
ORM3549  EQU   101                                                              
ORM5054  EQU   105                                                              
ORM5564  EQU   108                                                              
ORM65O   EQU   110                                                              
ORW1217  EQU   25                                                               
ORW1820  EQU   68                                                               
ORW2124  EQU   69                                                               
ORW2534  EQU   46                                                               
ORW3549  EQU   51                                                               
ORW5054  EQU   55                                                               
ORW5564  EQU   58                                                               
ORW65O   EQU   60                                                               
ORWWRK   EQU   65                                                               
         EJECT                                                                  
* FIELD DISPLACEMENTS FOR RATING SERVICE RECORDS                                
RRMETROA EQU   1                                                                
RRMETROB EQU   2                                                                
RRHOMES  EQU   3                                                                
RRV25    EQU   4                                                                
RRV611   EQU   5                                                                
RRM1217  EQU   6                                                                
*              7            ORIGINALLY 1517, BLANKED OUT                        
RRM1820  EQU   8                                                                
RRM2124  EQU   9                                                                
RRM2534  EQU   10                                                               
RRM3549  EQU   11                                                               
RRM5054  EQU   12                                                               
RRM5564  EQU   13                                                               
RRM65O   EQU   14                                                               
RRW1217  EQU   15                                                               
*              16           ORIGINALLY 1517, BLANKED OUT                        
RRW1820  EQU   17                                                               
RRW2124  EQU   18                                                               
RRW2534  EQU   19                                                               
RRW3549  EQU   20                                                               
RRW5054  EQU   21                                                               
RRW5564  EQU   22                                                               
RRW65O   EQU   23                                                               
RRWWRK   EQU   24                                                               
*                                                                               
RRECL    EQU   W_LMRECLQ                                                        
         EJECT                                                                  
         LTORG                                                                  
ITBELD   DSECT                                                                  
ITBELCO  DS    CL1                                                              
ITBELQ   EQU   X'31'                                                            
ITBELLEN DS    CL1                                                              
ITBELLO  DS    CL1                 LOWEST CATEGORY # OF THIS ELEMENT            
ITBELHI  DS    CL1                 HIGHEST CATEGORY # OF THIS ELEMENT           
ITBVALS  DS    0CL4                                                             
***********************************************************************         
       ++INCLUDE DELMDSECT                                                      
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DELMINTAB 10/06/14'                                      
         END                                                                    
