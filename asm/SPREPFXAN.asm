*          DATA SET SPREPFXAN  AT LEVEL 015 AS OF 03/11/93                      
*PHASE SPFX02LA                                                                 
*INCLUDE SORTER                                                                 
*                                                                               
***********************************************************************         
*                                                                               
*   TITLE     : FIX GREG'S DUMB MISTAKE ON 'L'-RECORDS                          
*                                                                               
*   COMMENTS  : NONE THAT GREG WOULD LIKE TO MAKE                               
*               *OFF-LINE PROCESS                                               
*                                                                               
*   OUTPUT    : BACK TO FILE                                                    
*                                                                               
*   REG USAGE : R0 - WORK.                                                      
*               R1 - WORK.                                                      
*               R2 -                                                            
*               R3 -                                                            
*               R4 -                                                            
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
         TITLE 'SPREPFXAN<===>SPFX02L : STATION-FILES FIX ON L-RECORDS'         
***********************************************************************         
*=========================== MAIN PROGRAM ============================*         
*                                                                               
SPFX02L  CSECT                                                                  
         PRINT NOGEN                                                            
         DS    8192C                                                            
         ORG   SPFX02L                                                          
         NMOD1 0,SPFX02L,R8                                                     
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    RQF                                                              
*                                                                               
EXIT     XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ MODE=REQFRST ===========================*         
RQF      XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
         LA    R2,KEY                                                           
         USING ANMRECD,R2                                                       
         MVI   ANMKTYPE,ANMKTYPQ   PROCESS TYPE-'L' ONLY                        
         DROP  R2                                                               
*                                                                               
         GOTO1 HIGHSTA             READ FIRST RECORD.                           
         B     RQF20                                                            
*                                                                               
RQF10    GOTO1 SEQSTA                                                           
*                                                                               
RQF20    L     R5,ADSTAT           ADSTAT-->RECORD FOUND.                       
         USING ANMRECD,R5                                                       
         CLI   ANMKTYPE,ANMKTYPQ   'L'-RECORD?                                  
         BNE   RQF200                NO, WE'RE DONE                             
*                                                                               
         L     R1,READCNT                                                       
         LA    R1,1(R1)            INCREMENT RECORDS READ.                      
         ST    R1,READCNT                                                       
*                                                                               
         CLC   ANMKAMRK,ZEROES     LOOK FOR BAD RECORDS                         
         BNE   RQF10                    NOT A BAD RECORD, READ NEXT             
         MVC   WORK(ANMKEYLQ),ANMKEYD   OTHERWISE, HOLD ONTO KEY                
         DROP  R5                                                               
*                                                                               
         LA    R4,KEY              CHECK MARKET RECORD FOR REAL                 
         XC    KEY,KEY               ALPHA MARKETS                              
         USING MKTRECD,R4                                                       
         LA    R5,WORK                                                          
         USING ANMRECD,R5                                                       
         MVI   MKTKTYPE,MKTKTYPQ   TYPE                                         
         MVC   MKTKMED,ANMKMED     MEDIA                                        
         MVC   MKTKMKT,ANMKNMRK    MARKET CODE                                  
         MVC   MKTKAGY,ANMKAGCY    AGENCY                                       
         DROP  R4,R5                                                            
*                                                                               
         GOTO1 HIGHSTA                                                          
         CLC   KEY(MKTKFILL-MKTKEY),KEYSAVE                                     
         BE    *+6                 THERE HAD BETTER BE A MARKET RECORD          
         DC    H'0'                 OUT THERE W/. INFO FROM L-RECORD            
*                                                                               
         L     R4,ADSTAT                                                        
         USING MKTRECD,R4                                                       
         OC    MKTALST(3),MKTALST                                               
         BZ    RQF50               NO NEED TO ADD PASSIVE POINTER               
         MVC   PRIMARK,MKTALST                                                  
         DROP  R4                                                               
*                                                                               
         L     R5,AREC             PASSIVE POINTER NEEDED                       
         MVC   0(ANMKEYLQ,R5),WORK                                              
         USING ANMRECD,R5                                                       
         MVC   ANMKAMRK,PRIMARK                                                 
         DROP  R5                                                               
         MVC   KEY(ANMKEYLQ),0(R5)                                              
         GOTO1 HIGHSTA                                                          
         CLC   KEY(ANMKEYLQ),KEYSAVE                                            
         BE    RQF50               PREVENT DUPLICATE KEYS                       
         GOTO1 ADD                                                              
*                                                                               
         MVC   P1(20),0(R5)        PRINT PASSIVE POINTER ADDED                  
         GOTO1 REPORT                                                           
         L     R1,ADDCNT           INCREASE # OF RECORDS ADDED                  
         LA    R1,1(R1)                                                         
         ST    R1,ADDCNT                                                        
*                                                                               
RQF50    XC    KEY,KEY             RESTORE ORIGINAL READ                        
         MVC   KEY,WORK                                                         
         GOTO1 HIGHSTA                                                          
         CLC   KEY(ANMKEYLQ),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,ADSTAT                                                        
         USING ANMRECD,R5                                                       
         OI    ANMCNTL,X'80'       MARK BAD RECORD FOR DELETION                 
         DROP  R5                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   RQF60                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',KEY,(R5)                      
RQF60    L     R1,DELCNT           INCREASE # OF RECORDS DELETED                
         LA    R1,1(R1)                                                         
         ST    R1,DELCNT                                                        
         B     RQF10               GET NEXT 'L'-RECORD                          
*                                                                               
*-------------------- PROCESSING END, SHOW QUOTAS --------------------*         
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
RQF200   MVC   P1(25),=C'NUMBER OF RECORDS READ = '                             
         EDIT  READCNT,(10,P1+27),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(25),=C'NUMBER OF "L" ADDED    = '                             
         EDIT  ADDCNT,(10,P1+27),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,     +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(25),=C'NUMBER OF "L" DELETED  = '                             
         EDIT  DELCNT,(10,P1+27),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,     +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
XRQF     GOTO1 AENDREQ                                                          
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*=========================== MISCELLANEOUS ===========================*         
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
** COUNTERS **                                                                  
ADDCNT   DS    F                   COUNTS # OF RECORDS ADDED                    
DELCNT   DS    F                   COUNTS # OF RECORDS DELETED                  
READCNT  DS    F                   COUNTS # OF 'L'-RECORDS READ                 
*                                                                               
ZEROES   DC    80C'0'                                                           
PRIMARK  DS    CL(L'ANMKAMRK)                                                   
MYWORK   DS    CL17                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== FIXED-RECORDS DSECT ========================*         
*                                                                               
       ++INCLUDE SPGENANMK                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= MARKET-RECORDS DSECT ========================*         
*                                                                               
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE SPREPMODES                                                     
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE SPREPWORKD                                                     
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREPFXAN 03/11/93'                                      
         END                                                                    
