*          DATA SET SPINF1D    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T21A1DA                                                                  
*INCLUDE CLUNPK                                                                 
         TITLE 'SINFO1D - COMMENT DISPLAY'                                      
         PRINT NOGEN                                                            
T21A1D   CSECT                                                                  
         NMOD1 00,T21A1D,RR=R8                                                  
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
*                                                                               
         USING FLDHDRD,R2                                                       
*                                                                               
         LA    R5,KEY                                                           
         USING COMRECD,R5                                                       
         ST    R8,RELO                                                          
*                                                                               
         LA    R0,REC              SET TO USE I/O 1                             
         ST    R0,AREC                                                          
         B     SETKEY                                                           
RELO     DC    A(0)                                                             
*                                                                               
SETKEY   XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY(13),PREVKEY                                                  
         XC    PREVKEY,PREVKEY                                                  
         GOTO1 HIGH                                                             
*                                                                               
         LA    R2,SINHDRH                                                       
         LA    RE,FLDDATA          HL1                                          
         LA    RF,LINLEN(RE)       HL2                                          
*                                                                               
         MVC   0(32,RE),=C'MED CLT PRD  EST STATION COMMENT'                    
         MVC   0(32,RF),=C'--- --- ---  --- ------- -------'                    
*                                                                               
         CLI   SVREC,X'1F'         TEST SDR                                     
         BE    *+16                                                             
         MVC   17(7,RE),SPACES                                                  
         MVC   17(7,RF),SPACES                                                  
*                                                                               
         OI    6(R2),X'80'         XMT HL1                                      
         LA    R2,LINLEN(R2)                                                    
         OI    6(R2),X'80'         XMT HL2                                      
*                                                                               
         LA    R2,LINLEN(R2)       POINT TO FIRST DISPLAY LINE                  
         LA    R4,13               MAX DISPLAY LINES                            
         SPACE 1                                                                
* NOTE THAT EST AND STATION ARE START AT VALUES, NOT FILTERS *                  
         SPACE 1                                                                
         LA    R7,3                SET LEN FOR 0D/0C/A-M/TYP                    
         OC    SVKEY+4(2),SVKEY+4  TEST CLT ENTERED                             
         BZ    HAVREC                                                           
         LA    R7,2(R7)                                                         
         OC    SVKEY+6(3),SVKEY+6  TEST PRD ENTERED                             
         BZ    HAVREC                                                           
         LA    R7,3(R7)                                                         
         B     HAVREC                                                           
         EJECT                                                                  
COMSEQ   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
HAVREC   DS    0H                                                               
         EX    R7,*+8              TEST FOR CONTROL BREAK                       
         B     *+10                                                             
         CLC   KEY(0),SVKEY        SAME 0D/0C/A-M/TYPE/....                     
         BNE   COMEND                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R5,AREC                                                          
         LA    R6,COMEL                                                         
*                                                                               
GETEL    CLI   0(R6),X'05'                                                      
         BE    HAVEL                                                            
         CLI   0(R6),X'15'                                                      
         BE    HAVEL                                                            
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    COMSEQ                                                           
         B     GETEL                                                            
         SPACE 1                                                                
         USING LINED,R2                                                         
         SPACE 1                                                                
HAVEL    DS    0H                                                               
         ZIC   RE,COMKAGY                                                       
         SLL   RE,28                                                            
         SRL   RE,28               ISOLATE MEDIA                                
         IC    RE,MEDTAB-1(RE)                                                  
         STC   RE,LMED                                                          
         B     HAVEL2                                                           
MEDTAB   DC    C'TRN....C'                                                      
*                                                                               
HAVEL2   DS    0H                                                               
         OC    COMKCLT,COMKCLT                                                  
         BZ    HAVELX                                                           
         MVC   LCLT,COMKCLT                                                     
         CLI   COMKCLT,C'*'        TEST OFFICE                                  
         BE    HAVELX                                                           
         GOTO1 =V(CLUNPK),DMCB,COMKCLT,LCLT,RR=RELO                             
*                                                                               
         OC    COMKPRD,COMKPRD                                                  
         BZ    *+14                                                             
         BAS   RE,GETPRD                                                        
         MVC   LPRD,WORK                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,COMKEST                                                     
         BZ    HAVELX                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST,DUB                                                         
*                                                                               
         OC    COMKSTA,COMKSTA                                                  
         BZ    HAVELX                                                           
*                                                                               
         CLI   SVKEY+3,C'M'        TEST MCOM                                    
         BNE   HAVEL4                                                           
         ICM   R0,3,COMKSTA+1      DISPLAY MARKET                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LSTA(4),DUB                                                      
         B     HAVELX                                                           
*                                                                               
HAVEL4   XC    WORK+10(2),WORK+10                                               
         MVC   WORK+12(3),COMKSTA                                               
         GOTO1 VMSUNPK,DMCB,WORK+10,DUB,WORK                                    
         MVC   LSTA(4),WORK                                                     
         CLI   WORK+4,C' '                                                      
         BE    HAVELX                                                           
         LA    R1,LSTA+3                                                        
         CLI   0(R1),C' '                                                       
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'-'                                                       
         MVC   2(1,R1),WORK+4                                                   
*                                                                               
HAVELX   DS    0H                                                               
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(79),ELEM                                                  
*                                                                               
         ZIC   RE,1(R6)            GET ELEMENT LENGTH                           
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),2(R6)                                                    
         MVC   LCOM,ELEM                                                        
*                                                                               
         OI    6(R2),X'80'         SET XMT                                      
         LA    R2,LINLEN(R2)                                                    
         MVC   PREVKEY,KEY                                                      
         BCT   R4,COMSEQ                                                        
         B     COMEXIT                                                          
         SPACE 2                                                                
COMEND   XC    PREVKEY,PREVKEY                                                  
*                                                                               
COMEXIT  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
         OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
* SUBROUTINE TO TRANSLATE BINARY PRD CODE TO EBCDIC                             
* OR DISPLAY PRODUCT GROUP CODE                                                 
* IF CLIENT HEADER NEEDED, READ IT INTO REC2                                    
         SPACE 1                                                                
GETPRD   NTR1                                                                   
         XC    WORK,WORK           CLEAR OUTPUT DISPLAY AREA                    
         CLI   COMKPRD,0           TEST PRDGRP PRESENT                          
         BNE   GETPRD20            YES                                          
*                                  *** PRODUCT CODE ***                         
         MVC   WORK2,KEY           SAVE CURRENT KEY                             
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),COMKAGY                                                 
         MVC   KEY+2(3),COMKCLT                                                 
         CLC   KEY(13),REC2        TEST HAVE CLTHDR ALREADY                     
         BE    GETPRD4                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,REC2             I/O TO REC2                                  
         ST    R0,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R0,REC              RESTORE TO I/O 1                             
         ST    R0,AREC                                                          
*                                                                               
GETPRD4  MVC   KEY(13),WORK2       RESTORE DIR FOR SEQ READING                  
         GOTO1 HIGH                                                             
         SPACE 1                                                                
* TRANSLATE BINARY CODE *                                                       
         SPACE 1                                                                
         LA    R1,REC2                                                          
         LA    R1,CLIST-CLTHDRD(R1)                                             
*                                                                               
GETPRD6  CLC   COMKPRD+2(1),3(R1)                                               
         BE    GETPRD8                                                          
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNL   GETPRD6                                                          
         LA    R1,=C'***'                                                       
*                                                                               
GETPRD8  MVC   WORK(3),0(R1)                                                    
         B     GETPRDX                                                          
         SPACE 1                                                                
* DISPLAY PRODUCT GROUP *                                                       
         SPACE 1                                                                
GETPRD20 MVC   WORK(1),COMKPRD                                                  
         UNPK  DUB(5),COMKPRD+1(3)                                              
         MVC   WORK+1(3),DUB                                                    
*                                                                               
GETPRDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* DSECT FOR DISPLAY LINE DATA *                                                 
         SPACE 1                                                                
LINED    DSECT                                                                  
         DS    CL8                 FLDHDR                                       
*                                                                               
         DS    CL1                                                              
LMED     DS    CL1                                                              
         DS    CL2                                                              
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LPRD     DS    CL4                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
         DS    CL1                                                              
LSTA     DS    CL7                                                              
         DS    CL1                                                              
LCOM     DS    CL55                                                             
*                                                                               
LINLEN   EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
COMRECD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPINF1D   05/01/02'                                      
         END                                                                    
