*          DATA SET ACCAP30    AT LEVEL 028 AS OF 07/26/17                      
*PHASE T61D30C                                                                  
*INCLUDE TIMETRN                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE XSORT                                                                  
*INCLUDE PERCALL                                                                
*INCLUDE ACGETRTE                                                               
*&&US                                                                           
*INCLUDE TMSUPD                                                                 
*&&                                                                             
*&&UK                                                                           
*INCLUDE TOBACCO                                                                
*&&                                                                             
* NSHE 024 09JUL04 MERGE UK AND US VERSIONS                                     
* JFOS 025 19JUL05 CHANGES TO SUPPORT ETIME ADJUSTMENTS                         
* TFRY 026 21JAN09 <BR21409L> BACK OUT OF MR#010198                             
         TITLE 'T61D30 - TIMESHEET MAINTENANCE'                                 
T61D30   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL TIMEGWSX-TIMEGWSD,*CAP30*,R7,CLEAR=YES,RR=R2                     
         LR    R9,RC                                                            
         USING TIMEGWSD,R9         R9=A(GLOBAL WORKING STORAGE)                 
         L     RC,0(R1)                                                         
         USING GEND,RC             RA=A(SAVED STORAGE)                          
         L     R8,ASYSD                                                         
         USING SYSD,R8             R8=A(SYSD)                                   
         ST    R2,BCRELO                                                        
         EJECT                                                                  
***********************************************************************         
* INITIALIZE TIMESHEET MODULE/RESOLVE ROUTINE ADDRESSES               *         
***********************************************************************         
         SPACE 1                                                                
INIT     DS    0H                                                               
         OI    GENSTAT1,RDUPAPPL   APPLICATION HANDLES RDUPDATE                 
         OI    GENSTAT4,USEAFRCH   CHECK KEY CHANGE ON PROTECTED SCRNS          
         OI    GENSTAT5,GENSELVR   ALWAYS GET VALREC FROM SELECT LIST           
         MVI   ACTELOPT,C'N'       DONT ADD X'F1' ACTIVITY ELEMENT              
         MVI   BCSPACES,C' '                                                    
         MVC   BCSPACES+1(L'BCSPACES-1),BCSPACES                                
         MVI   BCEFFS,X'FF'                                                     
         MVC   BCEFFS+1(L'BCEFFS-1),BCEFFS                                      
         ZAP   BCPZERO,=P'0'                                                    
*                                                                               
         ST    RC,BCSVRC           SAVE RC = A(GEND)                            
         LA    R1,LWS1CTAB                                                      
         AH    R1,=Y(L'LWS1CTAB)                                                
         ST    R1,AIOBC            A(GWS IOAREA)                                
         AH    R1,=Y(L'BCIO)                                                    
         ST    R1,AGOBLOCK         A(BCBLOCK)                                   
         L     R1,=V(TIMETRN)                                                   
         A     R1,BCRELO                                                        
         ST    R1,ATIMETRN         A(TIMETRN)                                   
         L     R1,=V(BINSRCH)                                                   
         A     R1,BCRELO                                                        
         ST    R1,ABINSRCH         A(BINSRCH2)                                  
         L     R1,=V(XSORT)                                                     
         A     R1,BCRELO                                                        
         ST    R1,AXSORT           A(XSORT)                                     
         L     R1,=V(PERCALL)                                                   
         A     R1,BCRELO                                                        
         ST    R1,APERCALL         A(PERCALL)                                   
         L     R1,=V(ACGETRTE)                                                  
         A     R1,BCRELO                                                        
         ST    R1,AGETRTE          A(GETRTE)                                    
*&&US                                                                           
         L     R1,=V(TMSUPD)                                                    
         A     R1,BCRELO                                                        
         ST    R1,ATMSUPD          A(TMSUPD)                                    
*&&                                                                             
*&&UK                                                                           
         L     R1,=V(TOBACCO)                                                   
         A     R1,BCRELO                                                        
         ST    R1,ATOBACCO         A(TOBACCO)                                   
*&&                                                                             
*                                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A5D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,XTSAR            A(TSAR)                                      
*                                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A63')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,AADDTRN          A(ADDTRN)                                    
*                                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A9F')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,APADDLE          A(PADDLE)                                    
*                                                                               
         XC    BCTID,BCTID         CLEAR TERMINAL ID                            
         GOTO1 SWITCH,DMCB,X'FFFFFFFF'                                          
         MVC   AUTL,0(R1)          A(UTL)                                       
         L     RF,AUTL                                                          
         MVC   BCTID,TSYM-UTLD(RF) TERMINAL ID #                                
*                                                                               
* LOAD A(COMMON OVERLAYS)                                                       
*                                                                               
         LA    R0,ACOMMONQ         # COMMON ROUTINES                            
         LA    R2,ACOMMONS                                                      
         LA    R3,X'31'            LOAD ACCAP31/ACCAP32/ACCAP33                 
INIT10   GOTO1 CALLOV,DMCB,((R3),0),0                                           
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,0(R2)                                                         
         LA    R2,L'ACOMMONS(R2)                                                
         LA    R3,1(R3)                                                         
         BCT   R0,INIT10                                                        
*                                                                               
* SET A(COMMON ROUTINES #1)                                                     
*                                                                               
INIT20   ICM   R1,15,ACOMMON1                                                   
         BZ    INIT30                                                           
         LA    R0,AROUTS1N                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ST    R1,AROUTS1(RE)      SET A(COMMON ROUTINE)                        
         STC   RF,AROUTS1(RE)      RF=OFFSET INTO COMMON(HIGH BYTE)             
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
* SET A(COMMON ROUTINES #2)                                                     
*                                                                               
INIT30   ICM   R1,15,ACOMMON2                                                   
         BZ    INIT40                                                           
         LA    R0,AROUTS2N                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ST    R1,AROUTS2(RE)      SET A(COMMON ROUTINE)                        
         STC   RF,AROUTS2(RE)      RF=OFFSET INTO COMMON(HIGH BYTE)             
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
* SET A(COMMON ROUTINES #3)                                                     
*                                                                               
INIT40   ICM   R1,15,ACOMMON3                                                   
         BZ    INIT50                                                           
         LA    R0,AROUTS3N                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ST    R1,AROUTS3(RE)      SET A(COMMON ROUTINE)                        
         STC   RF,AROUTS3(RE)      RF=OFFSET INTO COMMON(HIGH BYTE)             
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
INIT50   DS    0H                                                               
         MVC   BCKEYSAV,BIGKEY     CANNOT DESTROY BIGKEY BETWEEN                
*                                  TRANSACTIONS                                 
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACCKEY),BCSPACES     GET COMPANY RECORD                 
         MVC   BIGKEY(1),CMPY                                                   
         GOTO1 AGETACT,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(5,BCTODAY3),(1,BCTODAY3)                            
*                                                                               
         GOTO1 GETLDG,DMCB,C'1R'   GET 1R LEDGER STRUCTURE                      
         MVC   BC1RLEVS,ABCDLEN                                                 
         GOTO1 GETLDG,DMCB,C'SJ'   GET SJ LEDGER STRUCTURE                      
         MVC   BCSJLEVS,ABCDLEN                                                 
*&&UK                                                                           
         CLI   BCSJLEV3,7                                                       
         BNE   *+8                                                              
         MVI   BCSJLEV3,6          MAX JOB LENGTH IS 6                          
*&&                                                                             
         LA    RE,BCLEVS           SET EX A/C LVL LENGTHS                       
         LA    RF,BCXLEVS                                                       
         LA    R1,L'BCXLEVS                                                     
         SR    R0,R0                                                            
INIT54   IC    R0,BCLEVS-1(R1)                                                  
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AHI   R0,-1                                                            
         STC   R0,BCXLEVS-1(R1)                                                 
         BCT   R1,INIT54                                                        
                                                                                
         GOTO1 GETLDG,DMCB,C'1C'   GET 1C LEDGER STRUCTURE                      
         MVC   BC1CLEVS,ABCDLEN                                                 
         MVC   BCLNQS,BCLEVS                                                    
         GOTO1 AGETLNQS,BC1RLNQS   GET COMBINED LEVEL LENGTHS FOR 1R            
         GOTO1 AGETLNQS,BCSJLNQS   GET COMBINED LEVEL LENGTHS FOR SJ            
         GOTO1 AGETLNQS,BC1CLNQS   GET COMBINED LEVEL LENGTHS FOR 1C            
*                                                                               
         GOTO1 DICTATE,DMCB,C'L   ',TMSDICTL,BCDICTL                            
         GOTO1 DICTATE,DMCB,C'LU  ',TMSDICTU,BCDICTU                            
*                                                                               
         MVC   BIGKEY,BCKEYSAV     RESTORE BIGKEY TO ORIGINAL VALUE             
*                                                                               
         LA    R0,X'41'            TEMPO MAINTENANCE OVERLAY                    
         CLI   RECNUM,RTTHD        TEMPO HEADER                                 
         BE    INIT60                                                           
         CLI   RECNUM,RTTDT        TEMPO DETAIL                                 
         BE    INIT60                                                           
         CLI   RECNUM,RTTCM        TEMPO COMMUTER CODE                          
         BE    INIT60                                                           
         CLI   RECNUM,RTTLN        TEMPO LINE INFO                              
         BE    INIT60                                                           
*                                                                               
         LA    R0,X'40'            TIME DIS/CHA/ADD OVERLAY                     
         CLI   ACTEQU,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R0,X'42'            TIME LIST                                    
         CLI   ACTEQU,ACTREP                                                    
         BNE   *+8                                                              
         LA    R0,X'43'            TIME REPORT                                  
INIT60   GOTO1 CALLOV,DMCB,((R0),0),0                                           
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),0              CALL 40 OVERLAY                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DICTIONARY LITERAL POOL                                             *         
***********************************************************************         
         SPACE 1                                                                
TMSDICTL DS    0X                                                               
         DCDDL AC#TOTAL,5          TOTAL                                        
         DCDDL AC#SCRTO,12         SCREEN TOTAL                                 
         DC    X'00'                                                            
*                                                                               
TMSDICTU DS    0X                                                               
         DCDDL AC#TOTAL,5          TOTAL                                        
         DCDDL AC#CLIC,6           CLIENT                                       
         DCDDL AC#PROC,7           PRODUCT                                      
         DCDDL AC#JOBC,3           JOB                                          
         DCDDL AC#TASK,4           TASK                                         
         DCDDL AC#WC,3             WC (WORK CODE)                               
         DCDDL AC#MOA,3            MOA                                          
         DCDDL AC#DSCAD,3          D/A                                          
         DCDDL AC#CSTRT,6          COST RATE                                    
         DCDDL AC#OFFC,2           OF                                           
         DCDDL AC#ACTYD,3          AD                                           
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE ACBMONVALD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACCAP30GW                                                      
       ++INCLUDE ACCAP30DST                                                     
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACGOXBLOCK                                                     
       ++INCLUDE ACTIMETRND                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE FAUTL                                                          
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028ACCAP30   07/26/17'                                      
         END                                                                    
