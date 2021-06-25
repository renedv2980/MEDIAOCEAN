*          DATA SET DECBERR    AT LEVEL 002 AS OF 03/21/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DECBERRA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'FINAL CHECK FOR ERRORS IN THE OUTPUT CABLE FILE'                
DECABERR CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DECABERR,=V(REGSAVE)                                           
*                                                                               
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         B     INIT                                                             
*                                                                               
STXTAB   DS    0H                                                               
         DC    A(DECABERR)                                                      
         DC    V(PDUMPER)                                                       
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
*                                                                               
INIT     OPEN  (FILIN,(INPUT))                                                  
         GOTO1 =V(SORTER),DMCB,SORTCRD,RECCRD                                   
*                                                                               
         LA    R2,IOAREA                                                        
         USING SRTRECD,R2                                                       
         XC    SRTRECLN,SRTRECLN        CLEAR LENGTH                            
         XC    SRTKEY(SRTKEYLQ),SRTKEY  CLEAR SORT KEY                          
*                                                                               
CAB10    GET   FILIN,DEMOREC       GET DEMO RECORD                              
*                                                                               
         CLI   DEMRKEY,PMCODEQU    SORT ONLY Q-RECDS                            
         BE    CAB20                                                            
         CLI   DEMRKEY,PJCODEQU    AND J-RECDS WITH BOOK<>=0                    
         BE    CAB30                                                            
         B     CAB10                                                            
*                                                                               
CAB20    LA    R5,DEMRKEY          Q-RECD                                       
         USING PMKEY,R5                                                         
         MVC   SRTMED,PMMEDIA                                                   
         MVC   SRTSRC,PMSRC                                                     
         MVC   SRTBOOK,PMBOOK                                                   
         MVC   SRTSTAT,PMSTAT                                                   
         MVC   SRTBTYP,PMBTYP                                                   
         MVC   SRTFILN,PMPNUM                                                   
         MVI   SRTRTYP,1           SORT Q-RECD AFTER J-RECD                     
         SR    RF,RF                                                            
         B     CAB40                                                            
         DROP  R5                                                               
*                                                                               
CAB30    LA    R5,DEMRKEY                                                       
         USING PJKEY,R5                                                         
         OC    PJBOOK,PJBOOK       EXCLUDE J-RECS WITH BOOK=0                   
         BZ    CAB10                                                            
         OC    PJEXTNUM,PJEXTNUM   INCLUDE ONLY J-RECS W/ PROG NO               
         BZ    CAB10                                                            
         MVC   SRTMED,PJMEDIA                                                   
         MVC   SRTSRC,PJSRC                                                     
         MVC   SRTBOOK,PJBOOK                                                   
         MVC   SRTSTAT,PJSTAT                                                   
         MVC   SRTBTYP,PJBTYPE                                                  
         MVC   SRTFILN,PJINTNUM                                                 
         XC    SRTFILN,=X'FFFF'                                                 
         MVI   SRTRTYP,0           SORT J-RECD BEFORE Q-RECD                    
         B     CAB40                                                            
         DROP  R5                                                               
*                                                                               
CAB40    ICM   RF,3,DEMRECLN                                                    
         LA    RF,DEMOREC-SRTRECD(RF)                                           
         STCM  RF,3,SRTRECLN                                                    
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA     PUT REC TO SORTER             
         B     CAB10                                                            
*                                                                               
CAB100   CLOSE (FILIN,)                                                         
*                                                                               
         OPEN  (FILOUT,(OUTPUT))                                                
         MVI   PVRECTYP,0                                                       
*                                                                               
CAB110   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    CAB150                                                           
         L     R2,DMCB+4                                                        
         LA    R3,DEMOREC                                                       
         PUT   FILOUT,(R3)         PUT RECORD TO OUTPUT FILE                    
*                                                                               
* MAKE SURE THAT EACH J-RECD IS FOLLOWED BY A Q-RECD WITH A TIMELEM             
* (CONTROL RECORD THAT HOLDS THE 04 ELEM). THE SECOND Q-RECD SHOULD             
* MATCH THE PROGRAM NUMBER FROM THE J-RECORD.                                   
* THE SEQUENCE OF RECORDS EXPECTED IS                                           
*   1. J-POINTER                                                                
*   2. 1ST Q-RECORD (THE CONTROL RECORD)                                        
*      THIS RECORD SHOULD MATCH THE FILE NUMBER FROM THE J-POINTER              
*   3. 2ND Q-RECORD THAT HOLDS PROGRAM INFORMATION.                             
*      IT SHOULD MATCH THE PROGRAM NUMBER FROM THE J-POINTER                    
*   4+ ADDITIONAL Q-RECORDS                                                     
*                                                                               
         CLI   DEMRKEY,PJCODEQU    J-RECD                                       
         BNE   CAB120                                                           
         CLI   PVRECTYP,0                                                       
         BNE   ERROR1              Q-RECDS MISSING FOR PREVIOUS J-PTR           
         MVI   PVRECTYP,JRECQ                                                   
         MVC   SVJRECD,DEMRKEY     LATEST J-REC PRECEEDING Q-REC                
         B     CAB110                                                           
*                                                                               
CAB120   CLI   DEMRKEY,PMCODEQU    Q-RECD                                       
         BE    *+6                                                              
         DC    H'0'                UNEXPECTED RECORD TYPE                       
         CLI   PVRECTYP,JRECQ      PREVIOUS RECORD WAS THE J-POINTER            
         BNE   CAB130                                                           
         MVI   PVRECTYP,Q1RECQ     THIS SHOULD BE THE 1ST (CNTR) Q-RECD         
         USING PJKEY,SVJRECD                                                    
         MVC   HALF,PJINTNUM       MAKE SURE THE Q-RECD HAS SAME FILE#          
         XC    HALF,=X'FFFF'       AS THE PRECEEDING J-POINTER                  
         LA    RE,DEMRKEY                                                       
         USING PMKEY,RE                                                         
         CLC   PMPNUM,HALF         MATCH ON FILE NUMBER?                        
         BNE   ERROR1              NO.                                          
         DROP  RE                                                               
         LA    R6,DEMRKEY                                                       
         MVC   DATADISP,=AL2(KEYLENQ)                                           
         MVI   ELCODE,TIMCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   ERROR2              CONTROL RECORD EXPECTED                      
         B     CAB110                                                           
*                                                                               
CAB130   CLI   PVRECTYP,Q1RECQ     THIS SHOULD BE THE 2ND Q-RECD THAT           
         BNE   CAB140              HOLDS THE PROGRAM INFORMATION                
         MVI   PVRECTYP,0          RESET FLAG AFTER ALL REQUIRED RECDS          
         LA    R6,DEMRKEY                                                       
         MVC   DATADISP,=AL2(KEYLENQ)                                           
         MVI   ELCODE,PHTCODEQ     EXTRACT PROGRAM INFORMATION                  
         BAS   RE,GETEL                                                         
         BNE   ERROR3              PROGRAM INFORMATION MISSING                  
         USING PHTELEM,R6                                                       
         CLC   PHTDDS,PJEXTNUM+3   MATCH ON PROGRAM NUMBER?                     
         BNE   ERROR4              NO                                           
         B     CAB110                                                           
         DROP  R6                                                               
*                                                                               
CAB140   B     CAB110              SKIP ADDITIONAL Q-RECORDS                    
*                                                                               
CAB150   CLI   PVRECTYP,0                                                       
         BNE   ERROR1              Q-RECDS MISSING FOR LAST J-PTR               
*                                                                               
CABEXIT  GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLOSE FILOUT                                                           
         XBASE                                                                  
*                                                                               
*PRINT ERROR MESSAGES AND THE BAD RECORDS                                       
ERROR1   MVC   P,SPACES                                                         
         MVC   P(35),=C'Q-RECORDS MISSING FOR 1ST J-POINTER'                    
         B     ERRPRNT                                                          
ERROR2   MVC   P,SPACES                                                         
         MVC   P(31),=C'Q-RECD: CONTROL RECORD EXPECTED'                        
         B     ERRPRNT                                                          
ERROR3   MVC   P,SPACES                                                         
         MVC   P(36),=C'Q-RECD: PROGRAM INFORMATION EXPECTED'                   
         B     ERRPRNT                                                          
ERROR4   MVC   P,SPACES                                                         
         MVC   P(40),=C'J-POINTER VS Q-RECORD: PROGRAM MISSMATCH'               
         B     ERRPRNT                                                          
*                                                                               
ERRPRNT  GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         MVC   P(KEYLENQ),SVJRECD                                               
         GOTO1 =V(PRINTER)                                                      
         LA    RE,SVJRECD                                                       
         ST    RE,DMCB                                                          
         BAS   RE,PRNTHEX          FORMAT AND PRINT J-POINTER                   
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(KEYLENQ),DEMRKEY                                               
         GOTO1 =V(PRINTER)                                                      
         LA    RE,DEMRKEY                                                       
         ST    RE,DMCB                                                          
         BAS   RE,PRNTHEX          FORMAT AND PRINT CURRENT RECORD              
         DC    H'0'                DIE TO MAKE SURE THE NEXT STEP               
*        B     CABEXIT             (THE DATA LOAD) DOESN'T RUN                  
*                                                                               
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE PRINTS A LINE IN HEX FORMAT                                           
* DMCB(4) HAS ADDRESS OF RECORD TO PRINT                                        
***********************************************************************         
PRNTHEX  NTR1                                                                   
         L     R3,DMCB             A(RECORD TO FORMAT AND PRINT)                
         GOTO1 =V(HEXOUT),DMCB,(R3),WORK,KEYLENQ,=C'SEP'                        
         MVC   P,SPACES                                                         
         MVC   P(KEYLENQ),WORK           PRINT FIRST LINE (THE ZONES)           
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         MVC   P(KEYLENQ),WORK+KEYLENQ   PRINT FIRST LINE (THE DIGITS)          
         GOTO1 =V(PRINTER)                                                      
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
SORTCRD  DC    CL80'SORT FIELDS=(5,13,A),FORMAT=BI'                             
RECCRD   DC    CL80'RECORD TYPE=V,LENGTH=2017'                                  
*                                                                               
         EJECT                                                                  
FILIN    DCB   DDNAME=FILIN,DSORG=PS,RECFM=VB,MACRF=(GM),              X        
               EODAD=CAB100,LRECL=2000                                          
*                                                                               
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=VB,MACRF=(PM),             X        
               LRECL=2000,BLKSIZE=8200                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
PVRECTYP DS    X                                                                
JRECQ    EQU   1                                                                
Q1RECQ   EQU   2                                                                
*                                                                               
ELCODE   DS    X                                                                
DATADISP DS    H                                                                
*                                                                               
KEYLENQ  EQU   23                                                               
*                                                                               
SVJRECD  DS    CL23                                                             
IOAREA   DS    CL3000                                                           
         EJECT                                                                  
SRTRECD  DSECT                                                                  
SRTRECLN DS    AL4                 LENGTH OF SRTKEY + DEMO RECORD               
SRTKEY   DS    0X                                                               
SRTMED   DS    C                   MEDIA                                        
SRTSRC   DS    C                   SOURCE                                       
SRTBOOK  DS    XL2                 BOOK                                         
SRTSTAT  DS    CL5                 STATION                                      
SRTBTYP  DS    C                   BOOKTYPE                                     
SRTFILN  DS    XL2                 FILE NUMBER                                  
SRTRTYP  DS    C                   RECORD TYPE (J OR Q)                         
SRTKEYLQ EQU   *-SRTKEY                                                         
DEMOREC  DS    0X                  DEMO RECORD                                  
DEMRECLN DS    AL4                 DEMO RECORD LENGTH                           
DEMRKEY  DS    CL23                DEMO RECORD KEY                              
DEMRDATA DS    0X                  DEMO RECORD DATA                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DECBERR   03/21/14'                                      
         END                                                                    
