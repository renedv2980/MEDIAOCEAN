*          DATA SET REPCOPY    AT LEVEL 031 AS OF 05/01/02                      
*PHASE REPCOPYA,*                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE PRNTBL                                                                 
COPY     TITLE 'COPYING REP RECORDS'                                            
*                                                                               
*- REPCOPYA -- CREATE INTEREP MASTER RECORDS FROM TORBET                        
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  08/24/89  PJS  CLONED FROM REPCOPY AND MODIFIED                              
*                                                                               
REPCOPYA CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NBASE 0,*RCOPYA,=V(REGSAVE)                                            
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         SPACE                                                                  
*                                                                               
*- HOUSEKEEPING -- OPEN TAPE OUTPUT & DATAMGR INPUT.                            
         OPEN  (FILE,(OUTPUT))                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'REP',FLIST,REC                    
         EJECT                                                                  
*                                                                               
*- MAIN LOOP                                                                    
*                                                                               
         LA    R5,DDTABLE                                                       
MAIN100  EQU   *                                                                
         MVC   ID,DDKEYID(R5)      CURRENT ID                                   
         MVC   REPHI,DDREPHI(R5)   REP HI/LO SWITCH                             
         MVC   REPDISP,DDREP(R5)   DISP OF REP IN KEY                           
*                                                                               
         XC    COUNTERS,COUNTERS   SET ALL COUNTERS TO 0                        
*                                                                               
         BAS   RE,COPYREC                                                       
*                                                                               
         MVC   DDCOUNT(L'COUNTERS,R5),COUNTERS   SAVE COUNTS                    
*                                                                               
         LA    R5,DDLNTRY(R5)                                                   
         CLI   0(R5),0                                                          
         BNE   MAIN100             DO NEXT ENTRY                                
         SPACE                                                                  
*** END OF MAINLINE ***                                                         
         SPACE 2                                                                
*                                                                               
*- PRINT OUT STATS AT END OF RUN                                                
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(30),=CL30'REP COPY RUN COUNTERS'                           
*                                                                               
         MVC   MID1(6),=C'RECORD'                                               
         MVC   MID1+10(4),=C'KEYS'                                              
         MVC   MID1+20(4),=C'RECS'                                              
*                                                                               
         MVC   MID2(6),=C'------'                                               
         MVC   MID2+10(4),=C'----'                                              
         MVC   MID2+20(4),=C'----'                                              
*                                                                               
         MVC   LINE,MAXLINE        FORCE HEADLINE                               
         AP    LINE,=PL2'1'                                                     
*                                                                               
         LA    R5,DDTABLE                                                       
MAIN200  EQU   *                                                                
         MVC   P(8),DDLABEL(R5)                                                 
*                                                                               
         LA    R4,TOTS             TOTAL ACCUMS                                 
         LA    R6,P+10             A(PRINT LINE OUTPUT)                         
         LA    R7,2                2 THINGS TO EDIT                             
         LA    R8,DDCOUNT(R5)                                                   
MAIN220  EQU   *                                                                
         L     RF,0(R8)            NUMBER TO EDIT                               
*                                                                               
         L     RE,0(R4)            ADD TO ACCUMS                                
         AR    RE,RF                                                            
         ST    RE,0(R4)                                                         
*                                                                               
         EDIT  (RF),(6,(R6))                                                    
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R6,10(R6)           NEXT PRINT POSITION                          
         LA    R8,4(R8)                                                         
         BCT   R7,MAIN220                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R5,DDLNTRY(R5)                                                   
         CLI   0(R5),0                                                          
         BNE   MAIN200             DO NEXT ENTRY                                
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=CL17'NEW RECORD COUNT'                                    
****     EDIT  TOTS,(7,P+10)                                                    
         EDIT  TOTS+4,(6,P+20)                                                  
         GOTO1 =V(PRINTER)                                                      
         DROP  RA                                                               
*                                                                               
*- BACK TO CALLER.                                                              
         CLOSE (FILE,)                                                          
         GOTO1 =V(DATAMGR),DMCB,=C'DMCLSE',=C'REP',FLIST,REC                    
         XBASE                                                                  
         SPACE 2                                                                
FROMREP  DC    CL2'TO'             SOURCE REP CODE                              
*                                                                               
TOREP    DC    CL2'IR'             DESTINATION REP CODE                         
*                                                                               
ONE      DC    F'1'                                                             
*                                                                               
TOPFORM  DC    X'00',C'D'          SKIP TO NEW PAGE                             
SPACE2   DC    X'02',C'D'          SKIP 2 BEFORE PRINTING                       
         EJECT                                                                  
*                                                                               
*- COPYREC -- GENERIC RECORD COPY FROM 1 REP TO ANOTHER                         
*                                                                               
*  INPUT:  ID                      KEY ID TO COPY                               
*          REPHI                   REP HIGH/LOW IN KEY SWITCH                   
*          REPDISP                 DISPLACEMENT OF REP CODE IN KEY              
*          FROMREP                 SOURCE REP (OLD)                             
*          TOREP                   DESTINATION REP (NEW)                        
*                                                                               
*  OUTPUT: RECORDS WRITTEN TO OUTPUT FILE                                       
*                                                                               
COPYREC  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(1),ID           SEED KEY ID                                  
         CLI   REPHI,1                                                          
         BNE   CPY05               REP IS LOW.                                  
*                                                                               
         LA    RE,KEY              SEED HIGH REP CODE                           
         AH    RE,REPDISP                                                       
         MVC   0(2,RE),FROMREP                                                  
*                                                                               
CPY05    MVC   KEYSAVE,KEY                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY               
*                                                                               
CPY10    EQU   *                                                                
         L     R1,CNTKEY           BUMP KEY COUNTER                             
         A     R1,ONE                                                           
         ST    R1,CNTKEY                                                        
*                                                                               
         CLC   KEY(1),ID                                                        
         BNE   CPYEXT              DONE.                                        
         LA    RE,KEY                                                           
         AH    RE,REPDISP                                                       
         CLC   FROMREP,0(RE)       SOURCE REP?                                  
         BE    CPY15                                                            
*                                                                               
*- REP BREAK.  IF REP IS HI, WE'RE DONE.                                        
*  IF REP IS LOW, GET NEXT RECORD.                                              
         CLI   REPHI,1                                                          
         BE    CPYEXT              REP IS HIGH.  STOP                           
         B     CPY50               READ SEQ.                                    
*                                                                               
CPY15    EQU   *                                                                
         L     R1,CNTREC           BUMP REC COUNTER                             
         A     R1,ONE                                                           
         ST    R1,CNTREC                                                        
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'GETREC',=C'REPFIL',KEY+28,REC,MYWORK         
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,REC              PUT DESTINATION REP INTO RECORD              
         AH    RE,REPDISP                                                       
         MVC   0(2,RE),TOREP                                                    
*                                                                               
         SR    R8,R8               ADD 4 TO RECLEN (VARIABLE HEADER)            
         ICM   R8,3,REC+27                                                      
         A     R8,=F'4'                                                         
         STCM  R8,3,RECLEN                                                      
*                                                                               
         CLC   =F'10',CNTREC       DUMP 1ST 10 RECS TO PRINTER                  
*****    BL    CPY40                                                            
         B     CPY40                                                            
*                                                                               
         LA    R8,SPACE2           SKIP 2 LINES                                 
         CLC   =F'1',CNTREC                                                     
         BNE   CPY20                                                            
         LA    R8,TOPFORM          TOP OF FORM                                  
CPY20    EQU   *                                                                
         LH    R7,RECLEN           LENGTH OF RECORD                             
         PRINT GEN                                                              
         GOTO1 =V(PRNTBL),DMCB,=C'RECORDS',A(REC),C'DUMP',(R7),(R8)             
         PRINT NOGEN                                                            
*                                                                               
CPY40    LA    R0,RECLEN           WRITE TO OUTPUT FILE                         
         PUT   FILE,(R0)                                                        
*                                                                               
CPY50    GOTO1 =V(DATAMGR),DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEY               
         B     CPY10                                                            
*                                                                               
CPYEXT   XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- DATA DEFINITION TABLE -- REPOSITORY OF EVERYTHING MEANINGFUL                 
*                           FOR FINDING & PROCESSING DATA.                      
*                                                                               
*  - LAYOUT EQUATES SHOULD BE USED TO REFERENCE INDIVIUAL FIELDS !              
*                                                                               
DDKEYID  EQU   0    XL1            KEY ID                                       
DDREPHI  EQU   1    XL1            1=REP HI IN KEY.  0=REP LOW IN KEY           
DDREP    EQU   2    XL2            REP CODE DISPLACEMENT IN KEY                 
DDLABEL  EQU   4    CL8            ALPHA LABEL                                  
DDCOUNT  EQU   12   10F            RECORD COUNT FOR THIS ID                     
DDLNTRY  EQU   52                  LENGTH OF 1 DD TABLE ENTRY                   
         SPACE                                                                  
DDTABLE  DS    0F                                                               
*                                                                               
*- TEAM (DIVISION) RECORD                                                       
         DC    X'05',X'01',AL2(RTEMKREP-IO),CL8'TEAM',10F'0'                    
*                                                                               
DDTABLEX EQU   *-DDTABLE           LEN TESTER                                   
*                                                                               
*- GROUP RECORD                                                                 
         DC    X'07',X'01',AL2(RGRPKREP-IO),CL8'GROUP',10F'0'                   
*                                                                               
*- ADVERTISER RECORD                                                            
         DC    X'08',X'00',AL2(RADVKREP-IO),CL8'ADV',10F'0'                     
*                                                                               
*- PRODUCT RECORD                                                               
         DC    X'09',X'00',AL2(RPRDKREP-IO),CL8'PRD',10F'0'                     
*                                                                               
*- AGENCY RECORD                                                                
         DC    X'0A',X'00',AL2(RAGYKREP-IO),CL8'AGY',10F'0'                     
*                                                                               
*- CLASS RECORD                                                                 
         DC    X'0D',X'01',AL2(RCLSKREP-IO),CL8'CLASS',10F'0'                   
*                                                                               
*- CATAGORY RECORD                                                              
         DC    X'0F',X'01',AL2(RCTGKREP-IO),CL8'CATAGORY',10F'0'                
*                                                                               
*- OWNERSHIP RECORD                                                             
         DC    X'2A',X'01',AL2(ROWNKREP-IO),CL8'OWNER',10F'0'                   
*                                                                               
         DC    H'0'                END OF DATA DEFINITION TABLE                 
         EJECT                                                                  
DUMPLIST DS    0F                                                               
         DC    A(REPCOPYA,65000)                                                
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
UTL      DC    F'0',X'08'                                                       
SSB      DC    F'0'                                                             
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8' REPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X       '                                                    
*                                                                               
         DC    C'*MYWORK*'                                                      
MYWORK   DS    CL96                                                             
         DC    C'*DMCB*'                                                        
DMCB     DS    6F                                                               
         DC    C'*KEYS*'                                                        
KEY      DS    CL32                                                             
KEYSAVE  DS    CL32                                                             
*                                                                               
ID       DS    XL1                 CURRENT KEY ID                               
REPHI    DS    XL1                 1=REP IS HIGH; 0=REP IS LOW                  
REPDISP  DS    H                   DISPLACEMENT OF REP CODE IN KEY              
         DC    C'*COUNTERS*'                                                    
         DS    0F                                                               
COUNTERS DS    0XL40               10 FULL WORD COUNTERS                        
CNTKEY   DS    F                   # KEYS READ                                  
CNTREC   DS    F                   # RECORDS READ/WRITTEN                       
         DS    8F                  SPARE                                        
*                                                                               
TOTS     DS    10F                 TOTALS FOR ABOVE COUNTERS                    
*                                                                               
DUB      DS    D                                                                
WORK     DS    30F                                                              
         EJECT                                                                  
FILE     DCB   DDNAME=FILE,DSORG=PS,MACRF=(PM),                        X        
               RECFM=VB,LRECL=1000,BLKSIZE=1004                                 
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
         DS    0F                                                               
RECLEN   DC    X'00000000'                                                      
REC      DC    1000X'00'                                                        
         SPACE 2                                                                
IO       EQU   REC                 A SHORTHAND WAY TO SAY REC                   
         SPACE                                                                  
*                                                                               
*- ORG RECORDS TO REC TO GET LABELS.                                            
         PRINT OFF                                                              
         ORG   REC                                                              
       ++INCLUDE REGENTEM                                                       
         ORG   REC                                                              
       ++INCLUDE REGENGRP                                                       
         ORG   REC                                                              
       ++INCLUDE REGENADV                                                       
         ORG   REC                                                              
       ++INCLUDE REGENPRD                                                       
         ORG   REC                                                              
       ++INCLUDE REGENAGY                                                       
         ORG   REC                                                              
       ++INCLUDE REGENCLS                                                       
         ORG   REC                                                              
       ++INCLUDE REGENCTG                                                       
         ORG   REC                                                              
       ++INCLUDE REGENOWN                                                       
         PRINT ON                                                               
         SPACE 2                                                                
DPRINT   DSECT                                                                  
P        DS    CL132                                                            
HEAD1    DS    0CL132                                                           
         DS    CL34                                                             
TITLE    DS    CL60                                                             
         DS    CL38                                                             
MID1     DS    CL132                                                            
MID2     DS    CL132                                                            
         DS    5CL132              MID3/4/SUB1-3                                
SPACES   DS    CL132                                                            
SPACING  DS    CL4                                                              
LINE     DS    CL2                                                              
MAXLINE  DS    CL2                                                              
MONTHS   DS    CL36                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031REPCOPY   05/01/02'                                      
         END                                                                    
