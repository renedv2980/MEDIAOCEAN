*          DATA SET SPREPLU02  AT LEVEL 030 AS OF 08/29/00                      
*PHASE SPLU02A                                                                  
         TITLE 'USER MENU DEMO LISTING PGM'                                     
         PRINT NOGEN                                                            
SPLU02   CSECT                                                                  
         NMOD1 0,SPLU02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BE    RUN1                                                             
         CLI   MODE,CLTFRST                                                     
         BE    FORCE                                                            
DONE     MVI   MODE,CLTLAST                                                     
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RUN1     DS    0H                                                               
         B     EXIT                                                             
         SPACE 1                                                                
FORCE    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D28'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE      0D28/A-M                                     
         BNE   DONE                                                             
         B     GETIT                                                            
         SPACE 1                                                                
SEQU     DS    0H                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(5),KEYSAVE      0D28/A-M                                     
         BNE   DONE                                                             
GETIT    DS    0H                                                               
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
         L     R8,AREC                                                          
         USING USRDREC,R8                                                       
RECLOOP  DS    0H                                                               
         LA    R3,24(R8)           USREL FIELD                                  
         CLI   0(R3),1             IS IT AN 01 ELEMENT                          
         BNE   ERR1                                                             
         MVI   BYTE,X'FF'          SET NEW MENU FLAG FOR PRNT                   
         CLC   USRACTIV,TODAYB     IS IT TODAY'S DATE                           
         BNE   *+8                                                              
         MVI   P1+4,C'*'                                                        
         ZIC   R0,1(R3)            GET LENGTH OF ELEMENT                        
         AR    R3,R0               POINT TO 05 ELEMENT                          
         USING USRDEMEL,R3                                                      
         ST    R3,FULL             SAVE ADDR OF 1ST 05 ELEM                     
         XR    R7,R7               CLEAR REG TO COUNT ELEMS FOR SORT            
CNTELEMS DS    0H                                                               
         CLI   0(R3),0             COUNT                                        
         BE    SORTEM                    ELEMENTS                               
         LA    R7,1(R7)                           FOR                           
         ZIC   R0,1(R3)                               SORT                      
         AR    R3,R0                                                            
         B     CNTELEMS                                                         
         SPACE 1                                                                
SORTEM   DS    0H                                                               
         L     R3,FULL             GET ADDR OF 1ST 05 ELEMENT                   
         GOTO1 XSORT,DMCB,(R3),(R7),9,7,2                                       
*                                                                               
SETPRNT  LA    R5,P1               ADDR OF PRNT BUFFER                          
SETBCT   LA    R6,7                7 DEMOS ON A LINE                            
         LR    R7,R5               SAVE ADDR OF NEW OUTPUT LINE                 
*                                                                               
O5LOOP   DS    0H                                                               
         CLI   0(R3),0             IS IT END OF RECORD                          
         BE    PRNT                                                             
         CLI   0(R3),5             IS IT AN 05 ELEMENT                          
         BNE   ERR1                                                             
         MVC   5(7,R5),USRDEMS     MOVE IN DEMO RATING GROUP                    
         LA    R5,14(R5)           POINT TO NEXT DEMO FIELD                     
         ZIC   R0,1(R3)            LENGTH OF 05 ELEMENT                         
         AR    R3,R0               POINT TO NEXT 05 ELEMENT                     
         BCT   R6,O5LOOP                                                        
*                                                                               
         CLI   0(R3),0             NO MORE DEMOS                                
         BE    PRNT                                                             
         LR    R5,R7               POINT TO LINE JUST FILLED                    
         LA    R5,132(R5)          POINT TO NEXT LINE                           
         MVI   0(R5),0             SKIP IT                                      
         LA    R5,132(R5)          POINT TO NEXT AVAILABLE LINE                 
         C     R5,P14+132          IS IS END OF PRINT BUFFER                    
         BNL   PRNT                                                             
         B     SETBCT                                                           
         SPACE 1                                                                
PRNT     DS    0H                                                               
         CLI   BYTE,X'FF'          IS IT NEW MENU                               
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'       YES.  SKIP TO A NEW PAGE                     
         GOTO1 REPORT                                                           
         CLI   0(R3),0             END OF MENU                                  
         BE    SEQU                                                             
         MVI   BYTE,0              RESET PRINT HEADING FLAG                     
         LA    R5,P1               ROUTINE                                      
         LA    RE,14                      TO                                    
         XC    0(132,R5),0(R5)              CLEAR                               
         LA    R5,132(R5)                        PRINT                          
         BCT   RE,*-10                                BUFFER                    
         B     SETPRNT                                                          
         SPACE 1                                                                
ERR1     DS    0H                                                               
         MVC   P1(8),=C'BAD MENU'                                               
         GOTO1 REPORT                                                           
         B     SEQU                                                             
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
USRDRECD DSECT                                                                  
       ++INCLUDE SPGENUSRD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SPREPLU02 08/29/00'                                      
         END                                                                    
