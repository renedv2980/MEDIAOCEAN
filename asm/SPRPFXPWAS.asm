*          DATA SET SPRPFXPWAS AT LEVEL 032 AS OF 05/01/02                      
*PHASE SPFX026C                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'SPFX02 - SEARCH FOR MKT RECS W/ NOT STA LVL RECS'               
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 MYWORKL,SPFX02,R8,RR=R2,CLEAR=YES                                
         USING MYWORKD,RC                                                       
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
FX       DS    0H                                                               
         BAS   RE,INIT                                                          
*                                                                               
         XC    KEY,KEY                                                          
         XC    MKTKEY,MKTKEY                                                    
*                                                                               
         DS    0H                                                               
         MVC   KEY(L'PWKTYP),=X'0D7A'     PW RECORDS ONLY                       
         MVI   FLAG,MKTSRCH             SEARCHING FOR MKT LVL REC               
*                                                                               
FX032    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEY               
         B     FX040                                                            
*                                                                               
FX034    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'SPTDIR',KEY,KEY               
         B     FX040                                                            
                                                                                
*                                                                               
FX040    DS    0H                                                               
         CLI   DMCB+8,0                                                         
         BE    FX040X                                                           
         TM    DMCB+8,X'02'                                                     
         BNZ   FX040X                                                           
         DC    H'0'                                                             
FX040X   EQU   *                                                                
*                                                                               
         LA    R0,1                                                             
         A     R0,CNTDIRRD                                                      
         ST    R0,CNTDIRRD                                                      
*                                                                               
         USING PWRECD,R6                                                        
         LA    R6,KEY                                                           
         CLC   KEY(L'PWKTYP),=X'0D7A'                                           
         BNE   FX100                                                            
         OC    PWKSTA,PWKSTA            STATION LEVEL REC?                      
         BZ    FX050                    NO, CHK MKT REC                         
         MVI   FLAG,MKTSRCH             STATION REC FOUND, SET FLAG             
         B     FX034                    LOOK FOR NEXT MKT LVL REC               
*                                                                               
FX050    DS    0H                                                               
         CLI   FLAG,FNDMKT              MARKET PREVIOUSLY FOUND?                
         BNE   FX070                    NO, GETREC & SET FLAG                   
*                                                                               
         L     R6,AREC                  LOOK AT PREVIOUS MKT LVL REC            
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   FX070                    NO ELEM, RECORD OK, NEXT MKT?           
*                                                                               
         USING PWDOLEL,R6                                                       
         OC    PWDOLWG,PWDOLWG                                                  
         BZ    FX070                    FIELD ZERO, REC OK, NXT MKT?            
         DROP  R6                                                               
*                                                                               
* RECORD HAS PROBLEM, PRINT OUT PREVIOUS REC                                    
         DS    0H                  PRINT KEY                                    
         GOTO1 HEXOUT,DMCB,MKTKEY,WORK,18,=C'TOG'                               
         LA    R2,P1                                                            
         LA    R3,WORK                                                          
                                                                                
         MVC   0(2*L'PWFKEY,R2),0(R3)   PW KEY                                  
         LA    R2,2*L'PWFKEY+1(R2)                                              
         LA    R3,2*L'PWFKEY(R3)                                                
                                                                                
         MVC   0(2*1,R2),0(R3)          CONTROL BYTE                            
         LA    R2,2*1+1(R2)                                                     
         LA    R3,2*1(R3)                                                       
                                                                                
         MVC   0(2*4,R2),0(R3)          DISK ADDRESS                            
         GOTO1 REPORT                                                           
*                                                                               
         LA    R0,1                                                             
         A     R0,CNTPROB                                                       
         ST    R0,CNTPROB          INCREMENT COUNT OF RECS W/ PROBLEM           
*                                                                               
FX070    DS    0H                                                               
         MVC   MKTKEY,KEY                                                       
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'SPTFIL',             +        
               KEY+14,AREC,DMWORK                                               
         CLI   DMCB+8,0                                                         
         BE    FX070X                                                           
         TM    DMCB+8,X'02'                                                     
         BNZ   FX070X                                                           
         DC    H'0'                                                             
FX070X   EQU   *                                                                
                                                                                
         LA    R0,1                                                             
         A     R0,CNTFILRD                                                      
         ST    R0,CNTFILRD                                                      
*                                                                               
         DS    0H                                                               
         L     R6,AREC                                                          
         USING PWRECD,R6                                                        
*                                                                               
         CLC   PWFKEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FLAG,FNDMKT              MKT LVL REC FOUND, SET FLAG             
         B     FX034                                                            
         DROP  R6                                                               
*                                                                               
** NO MORE PW RECORDS **                                                        
*                                                                               
FX100    DS    0H                                                               
         B     FX200                                                            
         EJECT                                                                  
FX200    DS    0H                  REPORTING                                    
         MVI   LINE,99              PAGE EJECT                                  
                                                                                
         MVC   P1(25),=C'NUMBER OF DIRECTORY READS'                             
         L     R1,CNTDIRRD                                                      
         EDIT  (R1),(10,P1+30),ZERO=NOBLANK                                     
                                                                                
         MVC   P2(25),=C'NUMBER OF FILE      READS'                             
         L     R1,CNTFILRD                                                      
         EDIT  (R1),(10,P2+30),ZERO=NOBLANK                                     
                                                                                
         MVI   P3,0                                                             
         MVI   P4,0                                                             
                                                                                
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(29),=C'NUMBER OF RECORDS W/. PROBLEM'                         
         L     R1,CNTPROB                                                       
         EDIT  (R1),(10,P1+30),ZERO=NOBLANK                                     
                                                                                
         MVI   P2,0                                                             
         MVI   P3,0                                                             
         MVI   P4,0                                                             
                                                                                
         GOTO1 REPORT                                                           
*                                                                               
FXEND    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*=========================== INITIALIZATION ==========================*         
                                                                                
INIT     NTR1                                                                   
         LH    R1,=Y(NEWREC-MYWORKD)                                            
         LA    R1,MYWORKD(R1)                                                   
         ST    R1,ANEWREC                                                       
                                                                                
*                                                                               
         MVC   CBLSCMSK,=X'FFFF80'   SET CABLE SYSCODE MASK                     
                                                                                
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         MVC   AREC,ADPWREC                                                     
         SR    RF,RF                                                            
         DROP  RF                                                               
                                                                                
*                                                                               
         L     R0,ANEWREC                                                       
         LA    R1,L'NEWREC                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                 CLEAR NEW RECORD AREA                      
         XCEF  MKTREC,2000                                                      
         XCEF  DOLWKTAB,TABLNEQ                                                 
*                                                                               
         DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         EJECT                                                                  
***********************************************************************         
*======================= LOCAL WORKING STORAGE =======================*         
MYWORKD  DSECT                                                                  
                                                                                
*                                 ************* ADDRESSES *************         
ANEWREC  DS    A                                                                
                                                                                
*                                 ************** COUNTERS *************         
CNTDIRRD DS    F                   # OF DIRECTORY READS                         
CNTFILRD DS    F                   # OF FILE      READS                         
CNTPROB  DS    F                   # OF RECORDS WITH PROBLEMS                   
*                                 *********** MISCELLANEOUS ***********         
RELO     DS    F                                                                
CBLSCMSK DS    XL(L'BSTA)          MASK FOR CABLE STATION SYSCODE               
ELCODE   DS    XL1                                                              
FLAG     DS    XL1                                                              
MKTSRCH  EQU   X'00'                    LOOKING FOR MKT LVL REC                 
FNDMKT   EQU   X'01'                    FOUND MKT LVL REC                       
MKTKEY   DS    XL18                                                             
*                                                                               
*                                       DOLWK TABLE FOR X'06' ELEM'S            
DOLWKTAB DS    20XL38                   ENOUGH FOR 20 WK'S                      
TABEND   DC    X'FF'                    END OF TABLE                            
TABLNEQ  EQU   TABEND-DOLWKTAB                                                  
*                                                                               
NEWREC   DS    XL2000                                                           
MKTREC   DS    XL2000                                                           
*                                                                               
MYWORKX  EQU   *                                                                
MYWORKL  EQU   MYWORKX-MYWORKD                                                  
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                               
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPRPFXPWAS05/01/02'                                      
         END                                                                    
