*          DATA SET NEWRI49    AT LEVEL 017 AS OF 08/10/00                      
*PHASE T32049A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T32049 - DELETE PRODUCT REPORT'                                 
T32049   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEDP**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING MYD,R7                                                           
         ST    R2,RELO                                                          
         L     R2,ANETWS4          CLIENT REC IN ANETWS4                        
         ST    R2,NBACLI                                                        
         SPACE 1                                                                
*                                                                               
         CLI   MODE,VALREC                                                      
         BE    EDMOD                                                            
         CLI   MODE,PRINTREP                                                    
         BE    REPMOD                                                           
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE 3                                                                
EDMOD    NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRD,DMCB,SPLPRON                                               
         OI    SPLPRONH+6,X'80'                                                 
         MVC   XPROD,SPLPRO                                                     
*                                                                               
EDTX     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         EJECT                                                                  
REPMOD   NTR1                                                                   
         MVI   NBDATA,C'U'                                                      
RP10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    RP20                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BNE   RP10                                                             
         MVC   P+1(11),=C'UNITS FOUND'                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     RPX                                                              
*                                                                               
RP20     DS    0H                                                               
* GET NETGOAL ADDRS                                                             
         GOTO1 NBCALLOV,DMCB,0,X'D9000A35'                                      
         L     RF,DMCB             PICK UP ADDRESS OF NETGOAL                   
         ST    RF,ANETGOAL                                                      
*                                                                               
* READ GOAL RECS                                                                
         L     R5,ANETWS2                                                       
         USING NETGOALD,R5                                                      
         LA    R2,NETBLOCK                                                      
         ST    R2,NGANTBLK                                                      
         L     R2,ANETWS2                                                       
         ST    R2,NGAPLIST                                                      
         MVI   NGMAXPRD,200        LIST=200 PRODS MAX                           
         LA    R2,NETGOLHK                                                      
         ST    R2,NGAHOOK                                                       
         GOTO1 ANETGOAL,DMCB,NGBLOCK                                            
         B     RP40                                                             
*                                                                               
NETGOLHK NTR1                                                                   
         B     XIT                                                              
*                                                                               
RP40     L     R2,NGAPLIST                                                      
         L     R3,NGNPRDS                                                       
         USING LISTD,R2                                                         
         LTR   R3,R3                                                            
         BZ    RP60                                                             
RP42     CLC   LISTPRD,XPROD       ARE THERE GOALS FOR TARGET PROD              
         BE    RP50                                                             
         LA    R2,8(R2)                                                         
         BCT   R3,RP42                                                          
         B     RP60                                                             
RP50     DS    0H                  GOALS FOUND                                  
         MVC   P+1(11),=C'GOALS FOUND'                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     RPX                                                              
         EJECT                                                                  
RP60     DS    0H                  DELETE PRODUCT HEADER                        
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   KEY+4(3),XPROD                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RP65                                                             
         MVC   P+1(24),=C'PRODUCT HEADER NOT FOUND'                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     RPX                                                              
*                                                                               
RP65     MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         MVC   P+1(14),=C'PRODUCT HEADER'                                       
         L     R3,AIO                                                           
         GOTO1 HEXOUT,DMCB,0(R3),P+16,25                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
* DELETE PRODUCT HEADER                                                         
         CLI   SPLTEST,C'Y'                                                     
         BE    RP65B                                                            
         MVI   KEY+13,X'DD'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
         L     RE,AIO                                                           
         MVI   15(RE),X'C0'                                                     
         GOTO1 DATAMGR,=C'PUTREC',=CL8'SPTFILE',KEY+14,AIO,DMWORK               
*DELETE PRODUCT FROM CLIENT HEADER                                              
RP65B    XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R2,AIO                                                           
         USING CLTHDR,R2                                                        
         L     RF,ANETWS4          MOVE CLIST TO ANETWS4                        
         LA    R1,880                                                           
         LA    RE,CLIST                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
         LA    R3,220              (MAX NUMBER OF PRODS)                        
         L     RF,ANETWS4          (MAX NUMBER OF PRODS)                        
RP90     CLC   0(3,RF),XPROD       FIND MATCH                                   
         BE    RP100                                                            
         LA    RF,4(RF)                                                         
         BCT   R3,RP90                                                          
         MVC   P+1(23),=C'PROD NOT IN CLIENT LIST'                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     RP120                                                            
RP100    LA    RE,4(RF)            DELETE XPROD FROM CLIST                      
         LA    R1,880                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
         LA    RF,CLIST            RESET CLIST                                  
         L     RE,ANETWS4                                                       
         LA    R1,880                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
         CLI   SPLTEST,C'Y'                                                     
         BE    RP120                                                            
         GOTO1 DATAMGR,=C'PUTREC',=CL8'SPTFILE',KEY+14,AIO,DMWORK               
*                                                                               
RP120    DS    0H                  DELET PROD FROM PROD GROUPS                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D81'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(2),NBACTCLI                                                
         MVC   KEY+8(3),XPROD                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     *+16                                                             
RP123    MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
         CLC   KEY(5),KEYSAVE                                                   
         BNE   RPX                                                              
         CLC   KEY+8(3),KEYSAVE+8                                               
         BNE   RP123                                                            
         MVC   P+1(14),=C'PRDGRP PASSIVE'                                       
         GOTO1 HEXOUT,DMCB,KEY,P+16,20                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
* DELETE PRODUCT GROUP PASSIVE POINTER                                          
         CLI   SPLTEST,C'Y'                                                     
         BE    RP123                                                            
         MVI   KEY+13,X'DD'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
         B     RP123                                                            
RPX      B     XIT SAVE                                                         
         LTORG                                                                  
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
RELO     DS    A                                                                
ANETGOAL DS    A                                                                
XPROD    DS    CL3                 PROD TO BE DELETE                            
MYDLENE  EQU   *-MYD                                                            
*******                    ANY NEW FIELDS GO HERE                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NENETGOALD                                                     
       ++INCLUDE NEPLISTD                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID5D                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017NEWRI49   08/10/00'                                      
         END                                                                    
