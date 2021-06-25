*          DATA SET SPREPFX02C AT LEVEL 013 AS OF 05/01/02                      
*PHASE SPFX02C,+0                                                               
*INCLUDE SORTER                                                                 
                                                                                
***********************************************************************         
*                                                                               
*   COMMENTS  : CREATE WESTERN DBLBOOK RECORDS FROM BUY RECORDS                 
*                                                                               
*   INPUT     : SPTDIR & SPTFIL                                                 
*                                                                               
*   OUTPUT    : GLEE.DBLBK                                                      
*                                                                               
*   REG USAGE : R0 - WORK.                                                      
*               R1 - WORK.                                                      
*               R2 -                                                            
*               R3 - ADDRESSES ELEMENTS IN BUY RECORD                           
*               R4 - POINTS TO SORT RECORD                                      
*               R5 -                                                            
*               R6 -                                                            
*               R7 -                                                            
*               R8 - 2ND BASE REGISTER.                                         
*               R9 - 2ND REG USED BY SPWORKD.                                   
*               RA - 1ST REG USED BY SPWORKD.                                   
*               RB - BASE REGISTER.                                             
*               RC -                                                            
*               RD - REGISTER D CHAIN.                                          
*               RE -                                                            
*               RF -                                                            
***********************************************************************         
         TITLE 'SPREPFX02C<==>SPFX02 - CREATE WESTERN DBLBOOK RECORDS'          
***********************************************************************         
*================================ MAIN ===============================*         
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         DS    8192C                                                            
         ORG   *-8192                                                           
         NMOD1 0,SPFX02,R8,RR=R2                                                
         ST    R2,RELO                                                          
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
                                                                                
         CLI   MODE,REQFRST        OPEN STUFF                                   
         BE    FX10                                                             
         CLI   MODE,PROCBUY        PROCESS STUFF                                
         BE    FX20                                                             
         CLI   MODE,REQLAST        CLOSE STUFF                                  
         BE    FX100                                                            
*                                                                               
EXIT     XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================== REQFRST ==============================*         
                                                                                
FX10     DS    0H                                                               
         OPEN  (FILEOUT,OUTPUT)      SEQ DISK OUTPUT FILE                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         B     EXIT                                                             
                                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,17,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=17'                                    
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================== PROCBUY ==============================*         
                                                                                
FX20     DS    0H                                                               
         GOTO1 GETBUY                                                           
         L     R1,INRECS                                                        
         LA    R1,1(R1)                                                         
         ST    R1,INRECS                                                        
                                                                                
         L     R6,ADBUY            POINT TO RECORD WE JUST READ                 
         USING BUYRECD,R6                                                       
                                                                                
         XC    SREC,SREC           PREPARE TO BUILD SORT RECORD                 
*                                                                               
         MVC   SRSTA,BUYMSTA+2         STATION (ONLY)                           
         MVC   SRCLT,BUYKCLT           CLIENT                                   
         MVC   SREST,BUYKEST           ESTIMATE                                 
         MVC   SRLINE,BUYREC+10        LINE                                     
         MVC   SRSTIM,BDTIMST                                                   
         CLC   SRSTIM,=H'600'                                                   
         BNL   FX22                                                             
         SR    R0,R0                                                            
         ICM   R0,3,SRSTIM                                                      
         AH    R0,=H'2400'                                                      
         STCM  R0,3,SRSTIM                                                      
*                                                                               
FX22     MVC   SRETIM,BDTIMEND                                                  
         OC    SRETIM,SRETIM                                                    
         BZ    FX23                                                             
         CLC   SRETIM,=H'0600'                                                  
         BNL   FX24                                                             
         SR    R0,R0                                                            
         ICM   R0,3,SRETIM                                                      
         AH    R0,=H'2400'                                                      
         STCM  R0,3,SRETIM                                                      
         B     FX24                                                             
*                                                                               
FX23     SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,SRSTIM         GET START TIME                               
         D     R0,=F'100'          HOURS IN R1, MINUTES IN R0                   
         MH    R1,=H'60'           CONVERT HOURS TO MINUTES                     
         AR    R1,R0                                                            
         SR    R0,R0                                                            
         IC    R0,BDSEC            GET SPOTLENGTH                               
         AR    R1,R0                                                            
* CONVERT TO HHMM                                                               
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
         STCM  R1,3,SRETIM         SET END TIME                                 
         DROP  R6                                                               
*                                                                               
FX24     L     R6,ADBUY                                                         
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
*                                                                               
         USING REGELEM,R6                                                       
FX25     TM    RSTATUS,X'40'       IF ON                                        
         BO    EXIT                 THEN SKIP THIS 0B ELEMENT                   
         CLC   RDATE,=X'BC41'      TEST AFTER TO FEB01/94                       
         BNL   EXIT                YES - EXIT                                   
         MVC   SRDATE,RDATE         ELSE, PUT IN DATE                           
         GOTO1 DATCON,DMCB,(2,RDATE),(3,DUB)                                    
*                                                                               
         MVC   SRYEAR(2),DUB       MOVE YEAR/MONTH                              
         GOTO1 =V(SORTER),DMCB,=C'PUT',SREC                                     
         L     R1,SRTINS                                                        
         LA    R1,1(R1)                                                         
         ST    R1,SRTINS                                                        
                                                                                
         CLI   QOPT1,C'Y'          OPTION TO PRINT EVERYTHING\                  
         BE    *+12                                                             
         BC    0,EXIT                                                           
         OI    *-3,X'F0'                                                        
         BAS   RE,PRTREC           PRINT ONE BUY                                
         BAS   RE,PRTSORT          AND ASSOCIATED SORT RECORD                   
                                                                                
         B     EXIT                                                             
         DROP  R6                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================== REQLAST ==============================*         
                                                                                
FX100    DS    0H                                                               
         XC    SREC,SREC                                                        
         XC    SRECPREV,SRECPREV                                                
         LA    R6,BUFFER+4                                                      
         USING DBLBKREC,R6                                                      
                                                                                
FX110    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         BZ    FX115               PUT RECD IN FILE & CLOSE IT                  
                                                                                
         L     R1,SRTOUTS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,SRTOUTS                                                       
                                                                                
         MVC   SREC,0(R2)                                                       
         CLC   SREC(SRECMJRQ),SRECPREV                                          
         BE    FX130               GO BUILD ELEM & APPEND TO RECD               
                                                                                
         OC    SRECPREV,SRECPREV   IF 1ST TIME                                  
         BZ    FX120                THEN GO STRAIGHT TO BUILD KEY               
FX115    SR    R1,R1                                                            
         ICM   R1,3,DBLBKLEN       TAKE L(RECORD)                               
         LA    R1,1(R1)             ADD ONE FOR LAST BYTE                       
         CH    R1,=H'2000'                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         STCM  R1,3,DBLBKLEN                                                    
         LA    R1,4(R1)                                                         
         STCM  R1,3,BUFFER                                                      
         PUT   FILEOUT,BUFFER      PUT RECORD IN FILE                           
         L     R1,OUTRECS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,OUTRECS                                                       
                                                                                
         BC    0,FX115A                                                         
         OI    *-3,X'F0'                                                        
         ZICM  R0,DBLBKLEN,(3)                                                  
         GOTO1 PRNTBL,DMCB,=C'DBLBKREC',(R6),C'DUMP',(R0),=C'1D00'              
FX115A   LTR   R2,R2               AT SORTER END?                               
         BZ    FX140                YEP, GO CLOSE FILE                          
*                                                                               
FX120    LA    RE,BUFFER                                                        
         LA    RF,L'BUFFER                                                      
         XCEF                                                                   
         MVI   DBKTYPE,X'0D'       BUILD KEY                                    
         MVI   DBKSTYP,X'7B'                                                    
         MVI   DBKAGYMD,X'31'      FOR WESTERN                                  
         MVC   DBKYEAR,SRYEAR                                                   
         MVC   DBKMONTH,SRMONTH                                                 
         MVC   DBKSTA,SRSTA                                                     
         MVI   DBLBKLEN+1,24       RECORD LENGTH SO FAR                         
         MVC   DBLBKAGY,AGENCY                                                  
*                                                                               
FX130    MVC   SRECPREV,SREC                                                    
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING DBLBKEL,R4                                                       
         MVI   DBELCODE,X'05'      ELEM CODE                                    
         MVI   DBELLEN,16          ELEM LENGTH                                  
         MVC   DBELCLT,SRCLT                                                    
         MVI   DBELTYPE,C'B'                                                    
         MVC   DBELEST,SREST                                                    
         MVC   DBELLINE,SRLINE                                                  
         MVC   DBELDATE,SRDATE                                                  
         MVC   DBELSTIM,SRSTIM                                                  
         MVC   DBELNDTM,SRETIM                                                  
         DROP  R4                                                               
                                                                                
         LR    R2,R6               APPEND ELEM TO RECORD                        
         ZICM  R1,DBLBKLEN,(3)                                                  
         AR    R2,R1                                                            
         MVC   0(16,R2),ELEM                                                    
         LA    R1,16(R1)           UPDATE L(RECORD)                             
         STCM  R1,3,DBLBKLEN                                                    
         B     FX110                                                            
                                                                                
FX140    CLOSE (FILEOUT)                                                        
                                                                                
         LA    R2,BUCKTAB          PRINT COUNTER VALUES                         
FX150    CLI   0(R2),X'FF'                                                      
         BE    EXIT                                                             
         ICM   R3,15,0(R2)                                                      
         MVC   P(20),4(R2)                                                      
         EDIT  (R3),(7,P+22),COMMAS=YES,ZERO=NOBLANK                            
         GOTO1 REPORT                                                           
         LA    R2,L'BUCKTAB(R2)                                                 
         B     FX150                                                            
                                                                                
         DROP  R6                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*==================== PRINT HEX DUMP OF BUY RECORD ===================*         
PRTREC   NTR1                                                                   
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'INFOREC',(R6),C'DUMP',(R0),=C'1D00'               
         XIT1                                                                   
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*==================== PRINT HEX DUMP OF SORT RECORD ==================*         
PRTSORT  NTR1                                                                   
         GOTO1 PRNTBL,DMCB,=C'SORTREC',SREC,C'DUMP',17,=C'1D00'                 
         XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== GETEL ===============================*         
         GETEL R6,24,ELCODE                                                     
         SPACE 2                                                                
*================================ DCB ================================*         
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=VB,LRECL=2004,BUFNO=2,BLKSIZE=32760                        
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================= LTORG & CONSTANTS =========================*         
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
*------------------- TABLE OF RECORD COUNT BUCKETS -------------------*         
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
INRECS   DC    F'0',CL20'RECORDS IN'                                            
SRTINS   DC    F'0',CL20'SORT INPUTS'                                           
SRTOUTS  DC    F'0',CL20'SORT OUTPUTS'                                          
OUTRECS  DC    F'0',CL20'RECORDS OUT'                                           
         DC    X'FF'                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================= SORT RECORD DSECT =========================*         
         DS    0D                                                               
         DC    CL8'*SRTREC*'                                                    
SREC     DS    0XL17                                                            
SRYEAR   DS    XL1                 YEAR                                         
SRMONTH  DS    XL1                 MONTH                                        
SRSTA    DS    XL3                 STATION                                      
         DS    XL2                                                              
SRECMJRQ EQU   *-SREC                                                           
                                                                                
SRCLT    DS    XL2                 CLIENT                                       
SREST    DS    XL1                 ESTIMATE                                     
SRDATE   DS    XL2                 DATE                                         
SRSTIM   DS    XL2                 START TIME                                   
SRETIM   DS    XL2                 END TIME                                     
SRLINE   DS    XL1                 LINE                                         
SRECQ    EQU   *-SREC                                                           
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*============================ MY WORK AREA ===========================*         
         DS    0D                                                               
WORKAREA DC    CL8'WORKAREA'                                                    
RELO     DS    F                   RELO FACTOR                                  
ELCODE   DS    X                                                                
BOOK     DS    XL2                                                              
FIXSW    DS    C                                                                
SRECPREV DS    XL(L'SREC)                                                       
ELEM     DS    XL256                                                            
BUFFER   DS    XL2004                                                           
WORKQ    EQU   *-WORKAREA                                                       
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= DSECT FOR PRINT LINE ========================*         
PLINED   DSECT                                                                  
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================= SP DSECTS =============================*         
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENDBLBK                                                     
         EJECT                                                                  
* ++INCLUDE SPREPMODES                                                          
                                                                                
* ++INCLUDE SPREPWORKD                                                          
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPREPFX02C05/01/02'                                      
         END                                                                    
