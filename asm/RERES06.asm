*          DATA SET RERES06    AT LEVEL 020 AS OF 05/01/02                      
*PHASE T81906A,*                                                                
         TITLE 'T81906 - DEMO MENU LISTING'                                     
*                                                                               
*********************************************************************           
*                                                                   *           
*       RERES06 --- DEMO MENU LISTING (REPORT)                      *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* SEP25/90 (MRR) --- >CHANGE HEADLINE TO STANDARD                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
T81906   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1906**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
*                                                                               
         LH    RF,=Y(BUFF-SYSD)                                                 
         LA    RF,SYSD(RF)                                                      
         ST    RF,SBUFF                                                         
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 2                                                                
* CLEAR OUT PRINT LINES                                                         
         SPACE 1                                                                
         LA    R2,P1                                                            
         LA    R3,4                                                             
CLEAR    MVC   0(132,R2),SPACES                                                 
         LA    R2,132(R2)                                                       
         BCT   R3,CLEAR                                                         
         EJECT                                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,X'23'                                                        
         MVC   KEY+23(2),AGENCY    REP                                          
*                                                                               
         GOTO1 HIGH                                                             
         B     REP2                                                             
*                                                                               
REP1     GOTO1 SEQ                                                              
*                                                                               
REP2     CLC   KEY(25),KEYSAVE                                                  
         BNE   XIT                                                              
         SPACE 2                                                                
         GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         USING RDEMREC,R6                                                       
         LA    R5,P                                                             
         USING P1D,R5                                                           
         MVC   PCODE,RDEMKDEM      MENU CODE                                    
         MVC   PDESC,RDEMDES       MENU DESCRIPTION                             
*                                                                               
         DROP  R5,R6                                                            
         MVI   ELCODE,X'02'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RDEMDEL,R6                                                       
         LA    R2,RDEMDEM          POINT TO DEMO LIST                           
         LA    R3,12               DO 12 DEMOS AT A TIME                        
         LA    R5,P2                                                            
         USING P23D,R5                                                          
         LA    R4,PDEM1                                                         
         SPACE 2                                                                
         L     R7,SBUFF                                                         
         USING DBLOCKD,R7                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         SPACE 1                                                                
REP10    CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 DEMOCON,DMCB,(0,0(R2)),(6,0(R4)),(0,DBLOCKD),0                   
         CLI   1(R2),C'I'          REVERSE FUDGE FOR DEMOCON                    
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
         DROP  R7                                                               
         SPACE 1                                                                
         LA    R4,8(R4)            NEXT PRINT SPACE                             
         LA    R2,3(R2)            NEXT DEMO                                    
         CLI   0(R2),X'FF'         ANY MORE DEMOS                               
         BE    REP20                                                            
         BCT   R3,REP10            BUT ONLY DO 12 ON A LINE                     
         SPACE 1                                                                
         LA    R3,12                                                            
         LA    R5,132(R5)                                                       
         LA    R4,PDEM1                                                         
         B     REP10                                                            
         DROP  R5                                                               
         SPACE 1                                                                
REP20    BAS   RE,SPLAT                                                         
         B     REP1                                                             
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE DETAILS                                             
*                                                                               
HOOK     NTR1                                                                   
         SPACE 2                                                                
****AT THIS TIME, THERE ARE NO HEADLINE DETAILS.******                          
         SPACE 2                                                                
HOOKX    B     XIT                                                              
         EJECT                                                                  
*   PRINTING ROUTINE                                                            
         SPACE 2                                                                
SPLAT    NTR1                                                                   
         LA    R2,P1                                                            
         LA    R3,3                                                             
SPLAT20  CLC   0(110,R2),SPACES                                                 
         BE    SPLAT40                                                          
         MVC   P(110),0(R2)                                                     
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   0(110,R2),SPACES                                                 
SPLAT40  LA    R2,110(R2)                                                       
         BCT   R3,SPLAT20                                                       
*                                                                               
         GOTO1 SPOOL,PARAS,(R8)    SPACING LINE                                 
         B     XIT                                                              
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R6),34,ELCODE                                                   
         SPACE 2                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0,1                                                              
         PSPEC H1,001,AGYNAME                                                   
         PSPEC H1,058,C'DEMO MENU LISTING'                                      
         PSPEC H1,099,REPORT                                                    
         PSPEC H1,115,PAGE                                                      
         PSPEC H2,001,REQUESTOR                                                 
         PSPEC H2,058,17C'-'                                                    
         PSPEC H2,099,RUN                                                       
         PSPEC H4,001,C'CODE    DESCRIPTION'                                    
         PSPEC H5,001,C'           DEMOS'                                       
         PSPEC H6,001,C'----    -----------'                                    
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
* PRINT LINE DSECTS                                                             
         SPACE 2                                                                
P1D      DSECT                                                                  
PONE     DS    0CL132                                                           
         DS    CL1                                                              
PCODE    DS    CL2                                                              
         DS    CL5                                                              
PDESC    DS    CL60                                                             
         DS    CL64                                                             
         SPACE 3                                                                
P23D     DSECT                                                                  
P23      DS    0CL132                                                           
         DS    CL11                                                             
PDEM1    DS    CL6                                                              
         DS    CL2                                                              
         DS    CL113                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
* DSECT TO COVER SCREEN                                                         
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RERESF6D                                                       
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE            LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
SBUFF    DS    A                   A(START OF PRINT BUFFER)                     
         DS    CL(L'SYSSPARE-(*-SYSSPARE)) SPARE                                
*                                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020RERES06   05/01/02'                                      
         END                                                                    
