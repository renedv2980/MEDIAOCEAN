*          DATA SET NEWRI56    AT LEVEL 005 AS OF 10/23/08                      
*PHASE T32056A,+0                                                               
*INCLUDE CLUNPK                                                                 
*INCLUDE HELLO                                                                  
         TITLE 'T32056 - PROGRAM REC DELETE REPORT'                             
                                                                                
***************************************************************                 
*                                                                               
*        ALLOWS USER TO DELETE ONE OR ALL PROGRAM CODES                         
*        FOR A SPECIFIED NETWORK/DATE RANGE (JUN00 - SCHT)                      
*        EDIT MODULE                                                            
*                                                                               
***************************************************************                 
T32056   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEPRGD,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          ANETWS2+300=WORKING STORAGE                  
         A     R7,=F'300'                                                       
         USING WORKD,R7                                                         
*                                                                               
         CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BAS   RE,EDITMOD                                                       
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLNETH               NETWORK                                 
         NETGO NVNET,DMCB,MKTNUMB       MKT NUMBER SET TO MKTNUMB               
*                                                                               
         LA    R2,SPLEDTH               END DATE                                
         XC    PDATSTRT,PDATSTRT                                                
         XC    PDATEND,PDATEND                                                  
*                                                                               
         GOTO1 PERVAL,DMCB,(SPLEDTH+5,SPLEDT),MYDMWRK                           
         TM    DMCB+4,X'04'        ONLY 1 DATE INPUT                            
         BZ    EDINV                                                            
*                                                                               
         LA    RF,MYDMWRK                                                       
         USING PERVALD,RF                                                       
*                                                                               
         MVC   PDATEND,PVALCEND    END OF PERIOD                                
         DROP  RF                                                               
*                                                                               
         LA    R2,SPLPRGH               PROGRAM                                 
         CLC   8(3,R2),=C'ALL'                                                  
         BE    EDT60                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BE    EDINV                                                            
*                                                                               
*        CHECK IF PRGRAM REC EXISTS                                             
*                                                                               
EDT50    DS    0H                                                               
         NETGO NVSETSPT,DMCB                                                    
         OC    SPLPRG,SPACES      NUKPROG HAS SPACES AT END                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(2),MKTNUMB                                                 
         MVC   KEY+5(6),SPLPRG                                                  
         MVC   KEY+11(2),PDATEND                                                
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE           DOES PROGRAM REC EXIST ?               
         BNE   EDINV                     NO/ERROR                               
*                                                                               
EDT60    XC    FILENAME,FILENAME         YES/RESET TO UNIT FILE                 
         NETGO NVSETUNT,DMCB                                                    
*                                                                               
         MVI   TESTRUN,C'Y'                                                     
         LA    R2,SPLTESTH        TEST RUN                                      
         CLI   5(R2),0                                                          
         BE    EDINV                                                            
         CLI   SPLTEST,C'Y'                                                     
         BE    EDTX                                                             
         CLI   SPLTEST,C'N'        LIVE RUN                                     
         BNE   EDINV                                                            
         MVI   TESTRUN,C'N'                                                     
*                                                                               
EDTX     LA    R2,SPLNETH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*        WORKING STORAGE                                                        
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
COUNTER  DS    F                                                                
MKTNUMB  DS    H                   NETWORK MARKET NUMBER                        
PDATSTRT DS    CL2                 PROGRAM REC START DATE COMPRESSED            
PDATEND  DS    CL2                 PROGRAM REC END DATE COMPRESSED              
TESTRUN  DS    CL1                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICAD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEWRI56   10/23/08'                                      
         END                                                                    
